;;;; shader code
(defpackage transformations/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package transformations/shaders)

(input position :vec3 :location 0)
(input color :vec3 :location 1 :stage :vertex)
(input tex-coord :vec2 :location 2)
(output our-color :vec3 :stage :vertex)
(output -tex-coord :vec2 :stage :vertex)
(uniform transform :mat4)


(input our-color :vec3 :stage :fragment)
(input -tex-coord :vec2 :stage :fragment)
(output color :vec4 :stage :fragment)

(uniform our-texture :sampler-2d)
(uniform our-texture2 :sampler-2d)


(defun vertex ()
  (declare (stage :vertex))
  (setf gl-position (* transform (vec4  position 1))
        our-color color
        -tex-coord tex-coord))

(defun fragment ()
  (declare (stage :fragment))
  (setf color (mix (texture our-texture -tex-coord)
                   (texture our-texture2 -tex-coord)
                   0.2)))

;;;; back to normal lisp code
(in-package learning-opengl)

(defclass transformations (textures-combined)
  ()
   (:default-initargs
   :shader-parts '(:vertex-shader transformations/shaders::vertex
                   :fragment-shader transformations/shaders::fragment)))

(defmethod draw :before ((w transformations))
  ;; we use sb-cga for matrix/vector math
  (let ((transform (sb-cga:identity-matrix))
        (time (float (/ (get-internal-real-time)
                        internal-time-units-per-second))))
    ;; create the transformation
    (setf transform
          (sb-cga:matrix* (sb-cga:translate* 0.5 -0.5 0.0)
                          (sb-cga:rotate-around (sb-cga:vec 0.0 0.0 1.0)
                                                time)))
    ;; get matrix's uniform location and set matrix
    (gl:uniform-matrix-4fv (gl:get-uniform-location (shader-program w)
                                                    "transform")
                           transform
                           nil)))

#++
(common 'transformations)
