(in-package learning-opengl)

;;; reusing the definition of triangle1 where possible
(defclass triangle2 (triangle1)
  ())

(defun triangle2-build-vao (w vertex-data index-data)
  ;; cl-opengl has shortcuts to create 1 of most objects where the GL
  ;; API creates N at a time
  (let ((vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer))
        (vao (gl:gen-vertex-array)))
    ;; bind the Vertex Array Object first, then bind and set
    ;; vertex buffer(s) and attribute pointer(s)
    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer vbo)

    ;; I usually use the low-level wrappers in %GL: for things like
    ;; vertex buffers. For that we need to pass the data as a
    ;; C-style pointer. The static-vectors library is an easy way to
    ;; get lisp data that can also be passed as a pointer to
    ;; non-lisp APIs like GL, so using that here. (the other main
    ;; option is to manually manage the memory with CFFI's api)
    (static-vectors:with-static-vector (sv (length vertex-data)
                                           ;; for now assuming all floats
                                           :element-type 'single-float)
      ;; copy the vertex data into the static-vector
      (replace sv vertex-data)
      ;; and send it to GL. Package is %gl since we are using the
      ;; low-level API
      (%gl:buffer-data :array-buffer
                       ;; we don't have a convenient 'sizeof', so need
                       ;; to calculate the number of octets we are
                       ;; passing to GL manually
                       (* (length sv)
                          (cffi:foreign-type-size :float))
                       ;; pass the pointer for the static-vector
                       ;; array to GL
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    (gl:bind-buffer :element-array-buffer ebo)
    (static-vectors:with-static-vector (sv (length index-data)
                                           :element-type '(unsigned-byte 32))
      (replace sv index-data)
      (%gl:buffer-data :element-array-buffer
                       (* (length index-data)
                          (cffi:foreign-type-size :unsigned-int))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    ;; we pass GL_TRUE/GL_FALSE as CL booleans
    (gl:vertex-attrib-pointer 0 3 :float nil
                              (* 3 (cffi:foreign-type-size :float))
                              ;; cl-opengl translates integers to
                              ;; pointers for the functions like
                              ;; this that accept either an
                              ;; integer offset or a pointer
                              0)
    (gl:enable-vertex-attrib-array 0)

    ;; Note that this is allowed, the call to glVertexAttribPointer
    ;; registered VBO as the currently bound vertex buffer object so
    ;; afterwards we can safely unbind
    (gl:bind-buffer :array-buffer 0)

    ;; keep track of the buffer objects so we can delete them later
    (push vbo (buffers w))
    (push ebo (buffers w))

    ;; Unbind VAO (it's always a good thing to unbind any
    ;; buffer/array to prevent strange bugs)
    (gl:bind-vertex-array 0)
    ;; return the vao
    vao))

(defmethod init ((w triangle2))
  (setf (shader-program w)
        (triangle1-build-shader :vertex-shader 'triangle1/shaders::vertex
                                :fragment-shader 'triangle1/shaders::fragment))
  (setf (vao w)
        (triangle2-build-vao w
                             '(0.5 0.5 0.0   ; Top Right
                               0.5 -0.5 0.0  ; Bottom Right
                               -0.5 -0.5 0.0 ; Bottom Left
                               -0.5 0.5 0.0) ; Top Left
                             '(0 1 3         ; first triangle
                               1 2 3)        ; second triangle
                             )))

(defmethod draw ((w triangle2))
  ;; draw our first triangle
  (gl:use-program (shader-program w))
  (gl:bind-vertex-array (vao w))
  ;; using the low-level api from %gl again
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (gl:bind-vertex-array 0))

#++ (common 'triangle2)
