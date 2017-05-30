(in-package learning-opengl)

(defun radians (degrees)
  (float (* degrees (/ pi 180)) 1.0))

(defun get-uniforms (program names)
  (let ((h (make-hash-table)))
    (loop for name in names
          for loc = (gl:get-uniform-location
                     ;; simplify things by reusing the name
                     ;; translation from 3bgl-shaders, probably should
                     ;; be exported from there at some point
                     program (3bgl-shaders::translate-name name))
          do (setf (gethash name h) loc))
    h))


;;; some utilties to make it easier to get from the data structures
;;; produced by opticl to something accepted by GL

(defmacro expand-types ((object &rest types) &body body)
  `(typecase ,object
     ,@(loop for type in types
          collect `(,type
                    (locally (declare (type ,type ,object))
                      ,@body)))))

;;; NIL means pass image directly to GL, which will probably end up
;;; with it flopped since opticl considers the first pixel of data to
;;; be upper left, while GL considers it lower left.
(defparameter *flip-texture-y* t) ;; match GL coord by default

(defmacro do-pixels% ((j i wy) image &body body)
  ;; like DO-PIXELS, but with %PIXEL locally fbonud to flip the image vertically
  `(if *flip-texture-y*
       (opticl:do-pixels (,j ,i) ,image
         (macrolet ((%pixel (image-var y x)
                      `(opticl:pixel ,image-var (- ,',wy ,y 1) ,x)))
           ,@body))
       (opticl:do-pixels (,j ,i) ,image
         (macrolet ((%pixel (&rest r) `(opticl:pixel ,@r)))
           ,@body))))

(defun call-with-image-in-unsigned-bytes (image thunk)
  (cffi:with-foreign-object (p :unsigned-char (apply '* (array-dimensions image)))
    (opticl:with-image-bounds (wy wx c) image
      (macrolet ((v (c)
                   `(values
                     ,@(loop for i below c
                          collect `(cffi:mem-aref p :unsigned-char
                                                  (+ ,i (* i ,c) (* j wx ,c)))))))
        (ecase c
          (1 (expand-types (image opticl:1-bit-gray-image
                                  opticl:2-bit-gray-image
                                  opticl:4-bit-gray-image
                                  opticl:8-bit-gray-image)
               (do-pixels% (j i wy) image
                 (setf (v 1) (%pixel image j i))))
           (funcall thunk p :luminance wx wy))
          (3 (expand-types (image opticl:4-bit-rgb-image
                                  opticl:8-bit-rgb-image)
               (do-pixels% (j i wy) image
                 (setf (v 3) (%pixel image j i))))
           (funcall thunk p :rgb wx wy))
          (4 (expand-types (image opticl:4-bit-rgba-image
                                  opticl:8-bit-rgba-image)
               (do-pixels% (j i wy) image
                 (setf (v 4) (%pixel image j i))))
           (funcall thunk p :rgba wx wy)))))))

(defmacro with-image-in-unsigned-bytes ((image pointer-var gl-format-varr
                                         width-var height-var)
                                        &body body)
  `(call-with-image-in-unsigned-bytes
    ,image
    (lambda (,pointer-var ,gl-format-varr ,width-var ,height-var)
      ,@body)))

