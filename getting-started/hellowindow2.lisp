(in-package learning-opengl)

;;; subclass glop:window so we can add methods for this window type
(defclass hello2 (glop:window)
  ;; glop doesn't have an equivalent of glfwSetWindowShouldClose(),
  ;; so add it here
  ((window-should-close :initform nil :accessor window-should-close)))

;; if using the ON-FOO style of event handling, we need to specialize
;; at least these:
(defmethod glop:on-button ((w hello2) pressed button))
(defmethod glop:on-mouse-motion ((w hello2) x y dx dy))
(defmethod glop:on-resize ((w hello2) width height))
(defmethod glop:on-draw ((w hello2)))
(defmethod glop:on-close ((w hello2)))
;; for now we are just checking for escape key
(defmethod glop:on-key ((w hello2) pressed keycode keysym string)
  (format t "~s~%" keysym)
  (when (eql keysym :escape)
    (setf (window-should-close w) t)))

;; main entry point
(defun hello2 ()
  ;; glop:with-window creates a window and GL context with specified
  ;; parameters, and cleans up on exit
  (glop:with-window (w "LearnOpenGL" 800 600
                       ;; we ask for gl 3.3+ core profile
                       :major 3 :minor 3 :profile :core
                       ;; :WIN-CLASS 'HELLO2 tells it to use the class
                       ;; we defined earlier
                       :win-class 'hello2)
    ;; cl-opengl handles loading the function pointers, so no GLEW

    ;;define viewport dimensions
    (gl:viewport 0 0 (glop:window-width w) (glop:window-height w))

    ;; game loop
    (loop
      ;; see if we are done
      until (window-should-close w)
      ;; check for events and send them to ON-FOO handlers. Returns
      ;; NIL when we should exit loop. We add a "continue" restart so
      ;; we can keep going if an even handler errors.
      while (with-simple-restart (continue "continue")
              ;; use the default ON-FOO handlers this time
              (glop:dispatch-events w))
      do ;;; render
         ;; clear the colorbuffer.

         ;; cl-opengl uses mostly similar names to the C OpenGL API,
         ;; except with #\- in (hopfully) obvious places, like between words.
         (gl:clear-color 0.2 0.3 0.3 1)
         ;; GL constants are keywords, with #\- in place of #\_. Most
         ;; GL constants ending in "_BIT" can be used without the
         ;; "-bit" in cl-opengl
         (gl:clear :color-buffer)

         ;; swap the screen buffers
         (glop:swap-buffers w))))

#++
(hello2)
