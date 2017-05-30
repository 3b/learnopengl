(in-package learning-opengl)

 ;;;; mixin for tracking frame timing
(defclass delta-t ()
  ((delta-time :accessor delta-time :initform 0.0) ; time since last frame
   ;; single-float loses precision too fast, so keep timestamps as
   ;; double-float
   (last-frame :accessor last-frame :initform 0d0))); time of last frame


;;; gl 3.2+ includes a high res timer, so we will use that since we
;;; only care about time while we are actually rendering
(defun now ()
  "return current GL timestamp in seconds as double float (returns 0d0
if no valid GL 3.2+ context is available)"
  ;; single float runs out of precision, so use doubles here and cast to single
  ;; if needed after calculating a delta or whatever
  (/ (or (ignore-errors (gl:get* :timestamp)) 0d0)
     1000000000d0))

;; ideally we should update frame times right afterr swap-buffers (so
;; it is measured at the very beginning of a frame, but for now just
;; putting towards the start of a frame)
(defmethod draw :before ((w delta-t))
  (let ((current-frame (now)))
    (setf (delta-time w) (coerce (- current-frame (last-frame w))
                                 'single-float)
          (last-frame w) current-frame)))

;;;; mixin for tracking keyboard/mouse state
(defclass input-state ()
  ((keys :accessor keys :initform (make-hash-table))
   (dragging :accessor dragging :initform nil)
   (xoffset :accessor xoffset :initform 0)
   (yoffset :accessor yoffset :initform 0)
   (wheel-offset :accessor wheel-offset :initform 0)))

(defmethod glop:on-key :before ((w input-state)
                                pressed keycode keysym string)
  (setf (gethash keysym (keys w)) pressed))

(defmethod glop:on-button :before ((w input-state) pressed button)
  (case button
    (1
     (setf (dragging w) pressed))
    (4 (incf (wheel-offset w)))
    (5 (decf (wheel-offset w)))))

(defmethod glop:on-mouse-motion :before ((w input-state) x y dx dy)
  (cond
    ((dragging w)
     ;; accumulate multiple deltas in case we get more than 1 event in
     ;; a frame
     (incf (xoffset w) dx)
     (incf (yoffset w) (- dy)))
    (t
     ;; not bothering with mouse capture, so only update view when
     ;; dragging
     (setf (xoffset w) 0
           (yoffset w) 0))))

(defmethod draw :after ((w input-state))
  ;; reset mouse drag offsets at end of frame
  (setf (xoffset w) 0
        (yoffset w) 0
        (wheel-offset w) 0))

;;;; camera class

(defclass camera ()
  ( ;; configurable parameters
   (move-speed :accessor move-speed :initform 3.0 :initarg :speed)
   (mouse-sensitivity :accessor mouse-sensitivity :initform 0.25
                      :initarg :mouse-sensitivity)
   ;; internal state
   (pos :accessor pos :initform (sb-cga:vec 0.0 0.0 3.0))
   (front :accessor front :initform (sb-cga:vec 0.0 0.0 -1.0))
   (up :accessor up :initform (sb-cga:vec 0.0 1.0 0.0))
   (yaw :accessor yaw :initform -90)
   (pitch :accessor pitch :initform 0)
   (fov :accessor fov :initform 45.0 :initarg :fov)))

(defmethod update-camera (camera input-state dt)
  (let ((speed (* dt (move-speed camera)))
        (keys (keys input-state)))

    (incf (yaw camera) (* (mouse-sensitivity camera)
                          (xoffset input-state)))
    (setf (pitch camera) (alexandria:clamp (+ (pitch camera)
                                              (* (mouse-sensitivity camera)
                                                 (yoffset input-state)))
                                           -89 89))
    (unless (and (zerop (xoffset input-state))
                 (zerop (yoffset input-state)))
      (format t "yaw/pitch=~5,3f,~5,3f~%" (yaw camera) (pitch camera)))
    (setf (front camera)
          (sb-cga:normalize
           (sb-cga:vec (* (cos (radians (pitch camera)))
                          (cos (radians (yaw camera))))
                       (sin (radians (pitch camera)))
                       (* (cos (radians (pitch camera)))
                          (sin (radians (yaw camera)))))))


    (flet ((move (dir &optional (speed speed))
             (setf (pos camera)
                   (sb-cga:vec+ (pos camera)
                                (sb-cga:vec* dir speed)))))
      (when (gethash :w keys)
        (move (front camera)))
      (when (gethash :s keys)
        (move (front camera) (- speed)))
      (let* ((right (sb-cga:normalize (sb-cga:cross-product (front camera)
                                                            (up camera))))
             ;; local up vector
             (up (sb-cga:normalize (sb-cga:cross-product right
                                                         (front camera)))))
        (when (gethash :a keys)
          (move right (- speed)))
        (when (gethash :d keys) (move right))
        ;; adding e/q for up/down strafe too
        (when (gethash :e keys) (move up))
        (when (gethash :q keys) (move up (- speed))))
      )

    (incf (fov camera) (wheel-offset input-state))
    (setf (fov camera) (alexandria:clamp (fov camera) 1.0 45.0))))

(defun reset-camera (camera)
  ;; fixme: store initial values and don't repeat them here
  (setf (pos camera) (sb-cga:vec 0.0 0.0 3.0))
  (setf (front camera) (sb-cga:vec 0.0 0.0 1.0))
  (setf (up camera) (sb-cga:vec 0.0 1.0 0.0))
  (setf (yaw camera) -90.0)
  (setf (pitch camera) 0.0)
  (setf (fov camera) 45.0)
  (setf (mouse-sensitivity camera) 0.1))

(defmethod view-matrix ((camera camera))
  (kit.math:look-at (pos camera)
                    (sb-cga:vec+ (pos camera) (front camera))
                    (up camera)))

(defmethod projection-matrix ((camera camera) aspect
                              &key (near 0.1) (far 100))
  (kit.math:perspective-matrix (radians (fov camera))
                               aspect
                               near far))



(defclass camera-helper (delta-t input-state)
  ((current-camera :initform (make-instance 'camera) :accessor current-camera)))

(defparameter *w* nil)
(defmethod draw :before ((w camera-helper))
  (setf *w* w)
  (when (current-camera w)
    (update-camera (current-camera w) w (delta-time w))))

(defmethod glop:on-key :before ((w camera-helper) pressed keycode keysym string)
  ;; add a key to reset camera view
  (when (and (current-camera w) pressed
             (eql keysym :backspace))
    (reset-camera (current-camera w))))


(defmethod set-view-projection ((w camera-helper) program)
  (let ((view (gl:get-uniform-location program "view"))
        (projection (gl:get-uniform-location program "projection")))
    (gl:uniform-matrix-4fv view (view-matrix (current-camera w)) nil)
    (gl:uniform-matrix-4fv projection
                           (projection-matrix (current-camera w)
                                              (float
                                               (/ (glop:window-width w)
                                                  (glop:window-height w))))
                           nil)))
