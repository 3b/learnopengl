(in-package learning-opengl)

;;; subclass glop:window so we can add methods for this window type
(defclass hello (glop:window)
  ())

;; we can have glop send events either as a set of ON-FOO methods or
;; as a single ON-EVENT with specific event types. We don't care about
;; any events yet, so we will use the ON-EVENT style.

;; main entry point
(defun hello ()
  ;; glop:with-window creates a window and GL context with specified
  ;; parameters, and cleans up on exit
  (glop:with-window (w "LearnOpenGL" 800 600
                       ;; we ask for gl 3.3+ core profile
                       :major 3 :minor 3 :profile :core
                       ;; :WIN-CLASS 'HELLO tells it to use the class
                       ;; we defined earlier
                       :win-class 'hello)
    ;; cl-opengl handles loading the function pointers, so no GLEW

    ;;define viewport dimensions
    (gl:viewport 0 0 (glop:window-width w) (glop:window-height w))

    ;; game loop
    (loop
      ;; check for events and send them to handlers. Returns NIL when
      ;; we should exit loop. We add a "continue" restart so we can
      ;; keep going if an even handler errors.
      while (with-simple-restart (continue "continue")
              ;; :ON-FOO NIL tells glop to just send all events using
              ;; ON-EVENT
              (glop:dispatch-events w :on-foo nil))
      ;; swap the screen buffers
      do (glop:swap-buffers w))))

#++
(hello)
