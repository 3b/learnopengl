(in-package learning-opengl)

;; we'll be using the cube data a lot, so factor it out into a mixin
(defclass cube/uv ()
  ()
  (:default-initargs
   :vbo-data *cube/uv*
   ;; supplied cube data doesn't use indices, so just generate some
   ;; rather than modifying rendering code...
   :ebo-data (loop for i below (/ (length *cube/uv*) 8) collect i)
   :attributes '((0 3 8 0)
                 (1 3 8 3)
                 (2 2 8 6))))

(defclass coordinate-systems-with-depth (cube/uv textures-combined)
  ()
   (:default-initargs
    :shader-parts '(:vertex-shader coordinate-systems/shaders::vertex
                    :fragment-shader coordinate-systems/shaders::fragment)))

(defmethod clear ((w coordinate-systems-with-depth))
  (gl:clear-color 0.2 0.3 0.3 1)
  ;; Most places where C API uses bit constants combined with |,
  ;; cl-opengl accepts either a list of keywords in a single argument
  ;; or multiple keywords as &rest argument
  (gl:clear :color-buffer :depth-buffer))

(defmethod draw :before ((w coordinate-systems-with-depth))
  ;; cl-opengl lets us specify multiple options to a single call of
  ;; gl:enable or gl:disable, just list them as separate arguments.
  (gl:enable :depth-test)

  (let (model
        view
        projection
        (time (float (/ (get-internal-real-time)
                        internal-time-units-per-second))))
    ;; create the transformation
    (setf model (sb-cga:rotate-around (sb-cga:normalize
                                       (sb-cga:vec 0.5 1.0 0.0))
                                      (radians (* 50 time))))
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
     (gl:get-uniform-location (shader-program w) "projection") projection nil)))

#++
(common 'coordinate-systems-with-depth)
