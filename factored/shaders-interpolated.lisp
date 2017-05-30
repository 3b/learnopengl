;;;; shader code
#++(delete-package 'shaders-interpolated/shaders)
(defpackage shaders-interpolated/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package shaders-interpolated/shaders)

(input position :vec3 :location 0)
;; if we want to use the same name for different things in different
;; stages, we need to specify which we mean. If a stage isn't
;; specified, the same definition will be included in any stage that uses
;; the variable.
(input color :vec3 :location 1 :stage :vertex)
(output our-color :vec3 :stage :vertex)

(input our-color :vec3 :stage :fragment)
(output color :vec4 :stage :fragment)

;; uniforms allow more keywords:
;; (uniform name type &key stage location layout qualifiers default)


(defun vertex ()
  (declare (stage :vertex))
  (setf gl-position (vec4 position 1)
        our-color color))

(defun fragment ()
  (declare (stage :fragment))
  (setf color (vec4 our-color 1)))
;;;; back to normal lisp code
(in-package learning-opengl)
(setf 3bgl-shaders::*verbose* t)


(defclass shaders-interpolated (common2)
  ((uniform-location :initform nil :accessor uniform-location))
  (:default-initargs
   :shader-parts '(:vertex-shader shaders-interpolated/shaders::vertex
                   :fragment-shader shaders-interpolated/shaders::fragment)
   :vbo-data
   ;; Positions  ;; Colors
   '(0.5 -0.5 0.0  1.0 0.0 0.0            ; Bottom Right
     -0.5 -0.5 0.0  0.0 1.0 0.0           ; Bottom Left
     0.0 0.5 0.0  0.0 0.0 1.0             ; Top
     )
   :ebo-data '(0 1 2)
   :attributes '((0 3 6 0)
                 (1 3 6 3))))

#++
(common 'shaders-interpolated)
