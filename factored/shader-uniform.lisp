;;;; shader code

(defpackage shaders-uniform/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package shaders-uniform/shaders)

(input position :vec3 :location 0)
;; if we want to use the same name for different things in different
;; stages, we need to specify which we mean. If a stage isn't
;; specified, the same definition will be included in any stage that uses
;; the variable.
(input color :vec3 :location 1 :stage :vertex)

(output our-color :vec3 :stage :vertex)
(output color :vec4 :stage :fragment)

;; uniforms allow more keywords:
;; (uniform name type &key stage location layout qualifiers default)
(uniform our-color :vec4 :stage :fragment)

(defun vertex ()
  (setf gl-position (vec4 position 1)
        our-color color))

(defun fragment ()
  (setf color our-color))

;;;; back to normal lisp code
(in-package learning-opengl)


(defclass shader-uniforms (common2)
  ((uniform-location :initform nil :accessor uniform-location))
  (:default-initargs
   :shader-parts '(:vertex-shader shaders-uniform/shaders::vertex
                   :fragment-shader shaders-uniform/shaders::fragment)))


(defmethod init :after ((w shader-uniforms))
  (setf (uniform-location w)
        (gl:get-uniform-location (shader-program w) "ourColor")))

(defmethod draw :before ((w shader-uniforms))
  (let* ((time-value (float (/ (get-internal-real-time)
                               internal-time-units-per-second)))
         (green-value (/ (1+ (sin time-value)) 2)))
   ;; update the uniform
   (gl:uniformf (uniform-location w) 0 green-value 0 1)))

#++
(common 'shader-uniforms)
