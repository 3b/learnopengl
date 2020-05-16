(in-package learning-opengl)


;;;; General mesh data is slightly messy to deal with from CL code. CL
;;;; doesn't specify the memory layout of structures/classes or
;;;; vectors, so we can't just pass a pointer to a bunch of CL data to
;;;; GL. Instead, we need to assemble the various parts into a buffer
;;;; by hand (and can't easily use static-vectors as before, since we
;;;; might need multiple data types in 1 buffer).

;;;; This code will be using CFFI directly, which requires a bit of
;;;; care since unlike normal CL there is nothing keeping us from
;;;; writing over arbitrary memory.

;;; define a C-style structure (with known memory layout) for our
;;; vertex data:
(cffi:defcstruct vertex
  (position :float :count 3)
  (normal :float :count 3)
  (tex-coords :float :count 2)
  (tangent :float :count 3)
  (bitangent :float :count 3))

;; 

;;; texture descriptions can stay as CL data though
(defclass texture ()
  ((id :accessor id :initarg :id)
   ;; can't redefine cl:type
   (type :accessor texture-type :initarg :type)))

;; we don't bind a hash table for the cache by default. The values
;; would be invalid once we deleted the GL context, so we don't want
;; to keep them around if we open a new window without restarting
(defvar *texture-cache*)

(defmacro with-texture-cache (() &body body)
  `(let ((*texture-cache* (make-hash-table :test 'equalp)))
     ,@body
     (gl:delete-textures (alexandria:hash-table-values *texture-cache*))))

(defun load-texture-cached (file)
  (cond
    ((and (boundp '*texture-cache*) (gethash file *texture-cache*))
     ;; return result of test (= texture ID)
     )
    ((boundp '*texture-cache*)
     ;; load texture and return ID
     (setf (gethash file *texture-cache*)
           (load-texture file)))
    (t
     ;; no cache, just load it
     (load-texture file))))

(defclass mesh ()
  ((vertices :accessor vertices :initarg :vertices)
   (indices :accessor indices :initarg :indices)
   (vao :accessor vao)
   (textures :accessor textures :initarg :textures)
   (buffers :accessor buffers :initform nil)))

(defmethod initialize-instance :after ((m mesh) &key) ;; setupMesh
  (let ((vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer))
        (vao (gl:gen-vertex-array))
        ;; we can ask CFFI how big the struct is, and use that to
        ;; calculate the size of the data being sent
        (vertex-size (cffi:foreign-type-size '(:struct vertex))))
    (gl:bind-vertex-array vao)

    ;; we can keep using static-vectors for index data
    (gl:bind-buffer :element-array-buffer ebo)
    (static-vectors:with-static-vector (sv (length (indices m))
                                           :element-type '(unsigned-byte 32))
      (replace sv (indices m))
      (%gl:buffer-data :element-array-buffer
                       (* (length sv)
                          (cffi:foreign-type-size :unsigned-int))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    (gl:bind-buffer :array-buffer vbo)
    ;; allocate a temporary foreign buffer for the vertex data
    (cffi:with-foreign-object (p '(:struct vertex) (length (vertices m)))
      ;; copy the vertex data into the foreign buffer. (for now just
      ;; assuming a list of 3/2 element vectors in same order as c
      ;; struct)
      (loop for (position normal tex-coords tangent bitangent) in (vertices m)
            for i from 0
            for sp = (cffi:mem-aptr p '(:struct vertex) i)
            do (macrolet ((copy (var/slot count)
                            `(when ,var/slot
                               (cffi:lisp-array-to-foreign
                                ,var/slot
                                (cffi:foreign-slot-pointer sp '(:struct vertex)
                                                           ',var/slot)
                                '(:array :float ,count)))))
                 (copy position 3)
                 (copy normal 3)
                 (copy tex-coords 2)
                 (copy tangent 3)
                 (copy bitangent 3)))
      ;; and send it to GL.
      (%gl:buffer-data :array-buffer
                       (* (length (vertices m)) vertex-size)
                       p :static-draw))

    (macrolet ((offset (s)
                 `(cffi:foreign-slot-offset '(:struct vertex) ',s)))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil vertex-size (offset position))

      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 3 :float nil vertex-size (offset normal))

      (gl:enable-vertex-attrib-array 2)
      (gl:vertex-attrib-pointer 2 2 :float nil vertex-size (offset tex-coords))

      (gl:enable-vertex-attrib-array 3)
      (gl:vertex-attrib-pointer 3 3 :float nil vertex-size (offset tangent))

      (gl:enable-vertex-attrib-array 4)
      (gl:vertex-attrib-pointer 4 3 :float nil vertex-size (offset bitangent)))

    (gl:bind-buffer :array-buffer 0)

    (gl:bind-vertex-array 0)

    ;; keep track of the buffer objects so we can delete them later
    (setf (buffers m) (list vbo ebo)
          (vao m) vao)))

(defmethod draw-mesh ((m mesh) shader)
  (let ((diffuse# 0) ;; start at 0 so we can just return (incf x)
        (specular# 0)
        (normal# 0)
        (height# 0))
    (loop for i from 0
          for tex in (textures m)
          for name = (texture-type tex)
          for number = (case name
                         (:texture_diffuse
                          (incf diffuse#))
                         (:texture_specular
                          (incf specular#))
                         (:texture_normal
                          (incf normal#))
                         (:texture_height
                          (incf height#)))
          ;; cl-opengl accepts (or will soon) integers starting from 0
          ;; for :TEXTURE0, :TEXTURE1 etc in ACTIVE-TEXTURE
          do (gl:active-texture i)
             (gl:uniformi (gl:get-uniform-location
                              shader (format nil "~(~a~a~)" name number))
                             i)
             (gl:bind-texture :texture-2d (id tex)))
    (gl:bind-vertex-array (vao m))
    (%gl:draw-elements :triangles (length (indices m)) :unsigned-int 0)
    (gl:bind-vertex-array 0)
    (gl:active-texture :texture0))
)
