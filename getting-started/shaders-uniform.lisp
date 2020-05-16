;;; using 3bgl-shader for lisp-like shaders starting here, rather than
;;; storing c-like shaders in strings

;; types are keywords named like the glsl versions, :vec4, :float, etc.

;; built-in functions and variables have mixedCase names converted to
;; lisp-style names, like glPosition -> gl-position

;; 'swizzles' are written like (.xxyz foo) instead of GLSL's "foo.xxyz"

;; Types for globals (in/out/uniform/constant) must be specified,
;; while most types within functions are inferred from the types of
;; built-in functions and the types of the globals. In particular, a
;; single user-defined function can be used with any combination of
;; input/outputs that make sense for the body, and variants for each
;; combination used will be included automatically in the generated
;; glsl.

;;; 3bgl-shader has an API for defining shaders mixed in with normal
;;; lisp code, but easier to put it in its own package
(defpackage shaders-uniform/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package shaders-uniform/shaders)

;;; "in" variables are defined with (input name type &key location stage)
;; "layout (location = x)" is specified as :LOCATION X
(input position :vec3 :location 0)
;; if we want to use the same name for different things in different
;; stages, we need to specify which we mean. If a stage isn't
;; specified, the same definition will be included in any stage that uses
;; the variable.
(input color :vec3 :location 1 :stage vertex)

;; "out" variables use (output ...)
(output our-color :vec3 :stage :vertex)
(output color :vec4 :stage :fragment)

;; uniforms allow more keywords:
;; (uniform name type &key stage location layout qualifiers default)
(uniform our-color :vec4 :stage :fragment)

;; we give normal names to the "main" function, so can put a bunch in
;; same file without having any conflicts
(defun shaders-uniform-vertex ()
  (setf gl-position (vec4 position 1)
        our-color color))

(defun shaders-uniform-fragment ()
  (setf color1 our-color))


;;;; back to normal lisp code

(in-package learning-opengl)
;;; subclass glop:window so we can add methods for this window type
(defclass shaders-uniform (glop:window)
  ((window-should-close :initform nil :accessor window-should-close)))

;; we only care about one event type, so using ON-EVENT again
(defmethod glop:on-event ((w shaders-uniform) (event glop:key-press-event))
  (format t "~s~%" (glop:keysym event))
  (when (eql (glop:keysym event) :escape)
    (setf (window-should-close w) t)))

;; main entry point
(defun shaders-uniform ()
  ;; glop:with-window creates a window and GL context with specified
  ;; parameters, and cleans up on exit
  (glop:with-window (w "LearnOpenGL" 800 600
                       ;; we ask for gl 3.3+ core profile
                       :major 3 :minor 3 :profile :core
                       ;; :WIN-CLASS 'SHADERS-UNIFORM tells it to use the class
                       ;; we defined earlier
                       :win-class 'shaders-uniform)
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
      (gl:shader-source vertex-shader
                        ;; generate glsl from the lisp shader defined earlier
                        (3bgl-shaders:generate-stage
                         :vertex
                         'shaders-uniform/shaders::shaders-uniform-vertex))
      (gl:compile-shader vertex-shader)

      ;; check for compile time errors. gl:get-shader returns a
      ;; boolean for :compile-status
      (unless (gl:get-shader vertex-shader :compile-status)
        ;; and gl:get-shader-info-log returns a string directly
        (format t "ERROR::SHADER::VERTEX::COMPILATION_FAILED:~%~a~%"
                (gl:get-shader-info-log vertex-shader)))

;;; fragment shader
      ;; gl:shader-source accepts a string or list of strings
      (gl:shader-source fragment-shader
                        (3bgl-shaders:generate-stage
                         :fragment
                         'shaders-uniform/shaders::shaders-uniform-fragment))
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
        ;; 3bgl-shader converts lisp symbol OUR-COLOR to glsl name "ourColor"
        with uniform-location = (gl:get-uniform-location shader-program
                                                         "ourColor")
        until (window-should-close w)
        while (with-simple-restart (continue "continue")
                (glop:dispatch-events w :on-foo nil))
        for time-value = (float (/ (get-internal-real-time)
                                   internal-time-units-per-second))
        for green-value = (/ (1+ (sin time-value)) 2)
        do ;;; render
           ;; clear the colorbuffer.
           (gl:clear-color 0.2 0.3 0.3 1)
           (gl:clear :color-buffer)

           ;; be sure to activate the shader
           (gl:use-program shader-program)

           ;; update the uniform
           (gl:uniformf uniform-location 0 green-value 0 1)

           ;; draw the triangle
           (gl:bind-vertex-array vao)
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
(shaders-uniform)


