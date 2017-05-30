(in-package learning-opengl)

(defclass coordinate-systems-multiple-objects (cube/uv textures-combined)
  ((cube-positions :initform (list (sb-cga:vec  0.0  0.0  0.0)
                                   (sb-cga:vec  2.0  5.0 -15.0)
                                   (sb-cga:vec -1.5 -2.2 -2.5)
                                   (sb-cga:vec -3.8 -2.0 -12.3)
                                   (sb-cga:vec  2.4 -0.4 -3.5)
                                   (sb-cga:vec -1.7  3.0 -7.5)
                                   (sb-cga:vec  1.3 -2.0 -2.5)
                                   (sb-cga:vec  1.5  2.0 -2.5)
                                   (sb-cga:vec  1.5  0.2 -1.5)
                                   (sb-cga:vec -1.3  1.0 -1.5))
                   :accessor cube-positions))
  (:default-initargs
   :shader-parts '(:vertex-shader coordinate-systems/shaders::vertex
                   :fragment-shader coordinate-systems/shaders::fragment)))


(defmethod clear ((w coordinate-systems-multiple-objects))
  (gl:clear-color 0.2 0.3 0.3 1)
  ;; Most places where C API uses bit constants combined with |,
  ;; cl-opengl accepts either a list of keywords in a single argument
  ;; or multiple keywords as &rest argument
  (gl:clear :color-buffer :depth-buffer))

;; override DRAW directly rather than using :before method, since we
;; change the actual drawing...
(defmethod draw  ((w coordinate-systems-multiple-objects))
  (gl:enable :depth-test)

  (let (view
        projection
        (time (float (/ (get-internal-real-time)
                        internal-time-units-per-second))))
    ;; create transformations
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
     (gl:get-uniform-location (shader-program w) "view") view nil)
    (gl:uniform-matrix-4fv
     (gl:get-uniform-location (shader-program w) "projection") projection nil)
    (loop for position in (cube-positions w)
          for model = (sb-cga:matrix*
                       (sb-cga:translate position)
                       (sb-cga:rotate-around (sb-cga:normalize
                                              (sb-cga:vec 1.0 0.3 0.5))
                                             (radians (* 50 time))))
          do (gl:uniform-matrix-4fv
              (gl:get-uniform-location (shader-program w) "model") model nil)
             (%gl:draw-elements :triangles (length (ebo-data w)) :unsigned-int 0))))


#++
(common 'coordinate-systems-multiple-objects)
