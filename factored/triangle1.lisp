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
(defpackage triangle1/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package triangle1/shaders)

;;; "in" variables are defined with (input name type &key location stage)
;; "layout (location = x)" is specified as :LOCATION X
(input position :vec3 :location 0)

;; "out" variables use (output ...)
(output color :vec4)

;; we give normal names to the "main" function, so can put a bunch in
;; same file without having any conflicts
(defun vertex ()
  (setf gl-position (vec4 position 1)))

(defun fragment ()
  (setf color (vec4 1 0.5 0.2 1)))


;;;; back to normal lisp code


(in-package learning-opengl)

(defclass triangle1 (common1)
  ;; we need to track some state for the objects used to draw the
  ;; scene
  ((shader-program :initform nil :accessor shader-program)
   (vao :initform nil :accessor vao)
   (buffers :initform nil :accessor buffers)))

(defun triangle1-build-shader (stage entry-point &rest more-stages)
  (let ((shader-program (gl:create-program)))

;;; build and compile our shader program
    ;; for each stage specified, create a shader, compile it and add
    ;; to SHADER-PROGRAM
    (loop
      for (stage entry-point) on (list* stage entry-point more-stages) by #'cddr
      for shader = (gl:create-shader stage)
      ;; gl:shader-source accepts a string or list of strings. We use
      ;; 3bgl-shader to compile lispy code into glsl in a string
      do (gl:shader-source shader
                           (3bgl-shaders:generate-stage stage entry-point))
;;; try to compile the stage
         (gl:compile-shader shader)

         ;; check for compile time errors. gl:get-shader returns a
         ;; boolean for :compile-status
         (unless (gl:get-shader shader :compile-status)
           ;; and gl:get-shader-info-log returns a string directly
           (format t "ERROR::SHADER::~:@(~a~)::COMPILATION_FAILED:~%~a~%"
                   stage (gl:get-shader-info-log shader)))

         (gl:attach-shader shader-program shader)
         ;; we can tell GL to delete the shader once it is attached, GL
         ;; won't actually delete it until it is no longer attached to any
         ;; programs
         (gl:delete-shader shader))

;;; link the shader program
    (gl:link-program shader-program)
    ;; check for linking errors
    (unless (gl:get-program shader-program :link-status)
      (format t "ERROR::SHADER::PROGRAM::LINKING_FAILED:~%~a~%"
              (gl:get-program-info-log shader-program)))
    shader-program))


(defun triangle1-build-vao (w vertex-data)
  ;; cl-opengl has shortcuts to create 1 of most objects where the GL
  ;; API creates N at a time
  (let ((vbo (gl:gen-buffer))
        (vao (gl:gen-vertex-array)))
    ;; bind the Vertex Array Object first, then bind and set
    ;; vertex buffer(s) and attribute pointer(s)
    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer vbo)

    ;; I usually use the low-level wrappers in %GL: for things like
    ;; vertex buffers. For that we need to pass the data as a
    ;; C-style pointer. The static-vectors library is an easy way to
    ;; get lisp data that can also be passed as a pointer to
    ;; non-lisp APIs like GL, so using that here. (the other main
    ;; option is to manually manage the memory with CFFI's api)
    (static-vectors:with-static-vector (sv (length vertex-data)
                                           ;; for now assuming all floats
                                           :element-type 'single-float)
      (replace sv vertex-data)
      (%gl:buffer-data :array-buffer
                       (* (length sv)
                          (cffi:foreign-type-size :float))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw)

      ;; %gl since using the low-level API
      (%gl:buffer-data :array-buffer
                       ;; we do'nt have a convenient 'sizeof', so
                       ;; need to calculate the number of octets we
                       ;; are passing to GL manually
                       (* (length sv)
                          (cffi:foreign-type-size :float))
                       ;; pass the pointer for the static-vector
                       ;; array to GL
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

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

    ;; keep track of the VBO so we can delete it later
    (push vbo (buffers w))

    ;; Unbind VAO (it's always a good thing to unbind any
    ;; buffer/array to prevent strange bugs)
    (gl:bind-vertex-array 0)
    ;; return the vao
    vao))

(defmethod init ((w triangle1))
  (setf (shader-program w)
        (triangle1-build-shader :vertex-shader 'triangle1/shaders::vertex
                                :fragment-shader 'triangle1/shaders::fragment))
  (setf (vao w)
        (triangle1-build-vao w '(-0.5 -0.5 0.0 ; Left
                                 0.5 -0.5 0.0  ; Right
                                 0.0 0.5 0.0) ; Top
                             )))

(defmethod cleanup ((w triangle1))
  (gl:delete-buffers (buffers w))
  (gl:delete-vertex-arrays (list (vao w)))
  (gl:delete-program (shader-program w)))


(defmethod draw ((w triangle1))
  ;; draw our first triangle
  (gl:use-program (shader-program w))
  (gl:bind-vertex-array (vao w))
  (gl:draw-arrays :triangles 0 3)
  (gl:bind-vertex-array 0))

#++ (common 'triangle1)
