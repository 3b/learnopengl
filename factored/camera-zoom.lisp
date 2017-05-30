(in-package learning-opengl)

(defclass camera-zoom (coordinate-systems-multiple-objects camera-helper)
  ())


(defmethod draw  ((w camera-zoom))
  (gl:enable :depth-test)

  (let (view
        projection
        (time (float (/ (get-internal-real-time)
                        internal-time-units-per-second))))
    ;; create transformations
    (setf view (view-matrix (current-camera w)))
    ;; sb-cga doesn't include a perspective matrix, so we will get
    ;; that from mathkit
    (setf projection (projection-matrix (current-camera w)
                                        (float
                                         (/ (glop:window-width w)
                                            (glop:window-height w)))))
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
(common 'camera-zoom)
