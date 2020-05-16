(in-package learning-opengl)

;;; subclass glop:window so we can add methods for this window type
(defclass hello-triangle (glop:window)
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
(defmethod glop:on-event ((w hello-triangle) (event glop:key-press-event))
  (format t "~s~%" (glop:keysym event))
  (when (eql (glop:keysym event) :escape)
    (setf (window-should-close w) t)))

;; main entry point
(defun hello-triangle ()
  ;; glop:with-window creates a window and GL context with specified
  ;; parameters, and cleans up on exit
  (glop:with-window (w "LearnOpenGL" 800 600
                       ;; we ask for gl 3.3+ core profile
                       :major 3 :minor 3 :profile :core
                       ;; :WIN-CLASS 'HELLO-TRIANGLE tells it to use the class
                       ;; we defined earlier
                       :win-class 'hello-triangle)
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

      ;; I usually use the low-level wrappers in %GL: for things like
      ;; vertex buffers. For that we need to pass the data as a
      ;; C-style pointer. The static-vectors library is an easy way to
      ;; get lisp data that can also be passed as a pointer to
      ;; non-lisp APIs like GL, so using that here. (the other main
      ;; option is to manually manage the memory with CFFI's api)
      (static-vectors:with-static-vector (vertices 9
                                                   :element-type 'single-float)
        (replace vertices '(-0.5 -0.5 0.0 ; Left
                            0.5 -0.5 0.0  ; Right
                            0.0 0.5 0.0)) ; Top
        ;; bind the Vertex Array Object first, then bind and set
        ;; veretex buffer(s) and attribute pointer(s)
        (gl:bind-vertex-array vao)

        (gl:bind-buffer :array-buffer vbo)
        ;; %gl since using the low-level API
        (%gl:buffer-data :array-buffer
                         ;; we do'nt have a convenient 'sizeof', so
                         ;; need to calculate the number of octets we
                         ;; are passing to GL manually
                         (* (length vertices)
                            (cffi:foreign-type-size :float))
                         ;; pass the pointer for the static-vector
                         ;; array to GL
                         (static-vectors:static-vector-pointer vertices)
                         :static-draw)

        ;; we pass GL_TRUE/GL_FALSE as CL booleans
        (gl:vertex-attrib-pointer 0 3 :float nil
                                  (* 3 (cffi:foreign-type-size :float))
                                  ;; cl-opengl translates integers to
                                  ;; pointers for the functions like
                                  ;; this that accept either an
                                  ;; integer offset or a pointer
                                  0)
        (gl:enable-vertex-attrib-array 0)

        ;; Note that this is allowed, the call to
        ;; glVertexAttribPointer registered VBO as the currently bound
        ;; vertex buffer object so afterwards we can safely unbind
        (gl:bind-buffer :array-buffer 0)

        ;; Unbind VAO (it's always a good thing to unbind any
        ;; buffer/array to prevent strange bugs)
        (gl:bind-vertex-array 0))

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
           (gl:draw-arrays :triangles 0 3)
           (gl:bind-vertex-array 0)

           ;; swap the screen buffers
           (glop:swap-buffers w))

      ;; Properly de-allocate all resources once they've outlived their purpose
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo))
      (gl:delete-program shader-program)
)))

#++
(hello-triangle)
