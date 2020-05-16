(in-package learning-opengl)

;;; subclass glop:window so we can add methods for this window type
(defclass hello-triangle2 (glop:window)
  ((window-should-close :initform nil :accessor window-should-close)))

(defvar *vertex-shader-source* "
#version 330 core
layout (location = 0) in vec3 position;
void main()
{
   gl_Position = vec4(position.x, position.y, position.z, 1.0);
}")

(defvar *fragment-shader-source* "
#version 330 core
out vec4 color;
void main()
{
  color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}")

;; we only care about one event type, so using ON-EVENT again
(defmethod glop:on-event ((w hello-triangle2) (event glop:key-press-event))
  (format t "~s~%" (glop:keysym event))
  (when (eql (glop:keysym event) :escape)
    (setf (window-should-close w) t)))

;; main entry point
(defun hello-triangle2 ()
  ;; glop:with-window creates a window and GL context with specified
  ;; parameters, and cleans up on exit
  (glop:with-window (w "LearnOpenGL" 800 600
                       ;; we ask for gl 3.3+ core profile
                       :major 3 :minor 3 :profile :core
                       ;; :WIN-CLASS 'HELLO-TRIANGLE2 tells it to use the class
                       ;; we defined earlier
                       :win-class 'hello-triangle2)
    ;;define viewport dimensions
    (gl:viewport 0 0 (glop:window-width w) (glop:window-height w))

    ;; variables defined up front to avoid extra nesting from multiple
    ;; LETs
    (let ((vertex-shader (gl:create-shader :vertex-shader))
          (fragment-shader (gl:create-shader :fragment-shader))
          (shader-program (gl:create-program))
          ;; cl-opengl has shortcuts for making only 1 of most things
          ;; where the GL API makes N at a time
          (vbo (gl:gen-buffer))
          (ebo (gl:gen-buffer))
          (vao (gl:gen-vertex-array)))

;;; build and compile our shader program

;;; vertex shader
      ;; gl:shader-source accepts a string or list of strings
      (gl:shader-source vertex-shader *vertex-shader-source*)
      (gl:compile-shader vertex-shader)

      ;; check for compile time errors. gl:get-shader returns a
      ;; boolean for :compile-status
      (unless (gl:get-shader vertex-shader :compile-status)
        ;; and gl:get-shader-info-log returns a string directly
        (format t "ERROR::SHADER::VERTEX::COMPILATION_FAILED:~%~a~%"
                (gl:get-shader-info-log vertex-shader)))

;;; fragment shader
      ;; gl:shader-source accepts a string or list of strings
      (gl:shader-source fragment-shader *fragment-shader-source*)
      (gl:compile-shader fragment-shader)

      ;; check for compile time errors. gl:get-shader returns a
      ;; boolean for :compile-status
      (unless (gl:get-shader fragment-shader :compile-status)
        ;; and gl:get-shader-info-log returns a string directly
        (format t "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED:~%~a~%"
                (gl:get-shader-info-log fragment-shader)))

;;; link shaders
      (gl:attach-shader shader-program vertex-shader)
      (gl:attach-shader shader-program fragment-shader)
      (gl:link-program shader-program)
      ;; check for linking errors
      (unless (gl:get-program shader-program :link-status)
        (format t "ERROR::SHADER::PROGRAM::LINKING_FAILED:~%~a~%"
                (gl:get-program-info-log shader-program)))

      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader)

;;; set up vertex data (and buffer(s)) and attribute pointers

      ;; even with static-vectors, it is easier to specify the data in
      ;; a list or literal vector and copy later
      (let ((vertices '(0.5 0.5 0.0 ; Top Right
                        0.5 -0.5 0.0 ; Bottom Right
                        -0.5 -0.5 0.0 ; Bottom Left
                        -0.5 0.5 0.0 ; Top Left
                        ))
            (indices '(0 1 3 ; first triangle
                       1 2 3 ; second triangle
                       )))
        ;; bind the Vertex Array Object first, then bind and set
        ;; veretex buffer(s) and attribute pointer(s)
        (gl:bind-vertex-array vao)

        (gl:bind-buffer :array-buffer vbo)
        (static-vectors:with-static-vector (sv (length vertices)
                                               :element-type 'single-float)
          (replace sv vertices)
          (%gl:buffer-data :array-buffer
                           (* (length vertices)
                              (cffi:foreign-type-size :float))
                           (static-vectors:static-vector-pointer sv)
                           :static-draw))

        (gl:bind-buffer :element-array-buffer ebo)
        (static-vectors:with-static-vector (sv (length indices)
                                               :element-type '(unsigned-byte 32))
          (replace sv indices)
          (%gl:buffer-data :element-array-buffer
                           (* (length indices)
                              (cffi:foreign-type-size :unsigned-int))
                           (static-vectors:static-vector-pointer sv)
                           :static-draw))

        ;; we pass GL_TRUE/GL_FALSE as CL booleans
        (gl:vertex-attrib-pointer 0 3 :float nil
                                  (* 3 (cffi:foreign-type-size :float))
                                  0)
        (gl:enable-vertex-attrib-array 0)

        (gl:bind-buffer :array-buffer 0)

        ;; Unbind VAO (it's always a good thing to unbind any
        ;; buffer/array to prevent strange bugs) remember: do NOT
        ;; unbind the EBO, keep it bound to this VAO
        (gl:bind-vertex-array 0))

      ;; Uncommenting this call will result in wireframe polygons.
      ;; (gl:polygon-mode :front-and-back :LINE)

      ;; game loop
      (loop
        until (window-should-close w)
        while (with-simple-restart (continue "continue")
                (glop:dispatch-events w :on-foo nil))
        do ;;; render
           ;; clear the colorbuffer.
           (gl:clear-color 0.2 0.3 0.3 1)
           (gl:clear :color-buffer)

           ;; draw our first triangle
           (gl:use-program shader-program)
           (gl:bind-vertex-array vao)
           ;; need to use low-level API again
           (%gl:draw-elements :triangles 6 :unsigned-int 0)
           (gl:bind-vertex-array 0)

           ;; swap the screen buffers
           (glop:swap-buffers w))

      ;; Properly de-allocate all resources once they've outlived their purpose
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo ebo))
      (gl:delete-program shader-program)
      )))

#++
(hello-triangle2)
