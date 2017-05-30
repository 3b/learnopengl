;;;; shader code
(defpackage coordinate-systems/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package coordinate-systems/shaders)

(input position :vec3 :location 0)
(input color :vec3 :location 1 :stage :vertex)
(input tex-coord :vec2 :location 2)
(output our-color :vec3 :stage :vertex)
(output -tex-coord :vec2 :stage :vertex)

(uniform model :mat4)
(uniform view :mat4)
(uniform projection :mat4)


(input our-color :vec3 :stage :fragment)
(input -tex-coord :vec2 :stage :fragment)
(output color :vec4 :stage :fragment)

(uniform our-texture :sampler-2d)
(uniform our-texture2 :sampler-2d)


(defun vertex ()
  (declare (stage :vertex))
  (setf gl-position (* projection view model (vec4 position 1))
        our-color color
        -tex-coord tex-coord))

(defun fragment ()
  (declare (stage :fragment))
  (setf color (mix (texture our-texture -tex-coord)
                   (texture our-texture2 -tex-coord)
                   0.2)))

;;;; back to normal lisp code
(in-package learning-opengl)

(defclass coordinate-systems (textures-combined)
  ()
   (:default-initargs
   :shader-parts '(:vertex-shader coordinate-systems/shaders::vertex
                   :fragment-shader coordinate-systems/shaders::fragment)))

(defmethod draw :before ((w coordinate-systems))

  (let (model
        view
        projection
        (time (float (/ (get-internal-real-time)
                        internal-time-units-per-second))))
    ;; create the transformation
    (setf model (sb-cga:rotate-around (sb-cga:vec 1.0 0.0 0.0)
                                      (radians -55)))
    (setf view (sb-cga:translate* 0.0 0.0 -3.0))
    ;; sb-cga doesn't include a perspective matrix, so we will get
    ;; that from mathkit
    (setf projection (kit.math:perspective-matrix (radians 45)
                                                  (float
                                                   (/ (glop:window-width w)
                                                      (glop:window-height w)))
                                                  0.1 100))
    ;; get matrix's uniform location and set matrix
    (gl:uniform-matrix-4fv
     (gl:get-uniform-location (shader-program w) "model") model nil)
    (gl:uniform-matrix-4fv
     (gl:get-uniform-location (shader-program w) "view") view nil)
    (gl:uniform-matrix-4fv
     (gl:get-uniform-location (shader-program w) "projection") projection nil))

)

#++
(common 'coordinate-systems)
