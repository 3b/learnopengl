(in-package learning-opengl)

;; factoring out some common things, so it doesn't need to be included
;; with every version.

(defclass common1 (glop:window)
  ;; glop doesn't have an equivalent of glfwSetWindowShouldClose(),
  ;; so add it here
  ((window-should-close :initform nil :accessor window-should-close)))

;; if using the ON-FOO style of event handling, we need to specialize
;; at least these:
(defmethod glop:on-button ((w common1) pressed button)
  (format t "~s ~s~%" button pressed))
(defmethod glop:on-mouse-motion ((w common1) x y dx dy))
(defmethod glop:on-resize ((w common1) width height))
(defmethod glop:on-draw ((w common1)))
(defmethod glop:on-close ((w common1)))
;; for now we are just checking for escape key
(defmethod glop:on-key ((w common1) pressed keycode keysym string)
  (format t "~s ~s~%" keysym pressed)
  (when (eql keysym :escape)
    (setf (window-should-close w) t)))

(defmethod draw ((w common1))

)

(defmethod clear ((w common1))
  ;; factor out the clearing so we can modify it later

  ;; cl-opengl uses mostly similar names to the C OpenGL API,
  ;; except with #\- in (hopfully) obvious places, like between words.
  (gl:clear-color 0.2 0.3 0.3 1)
  ;; GL constants are keywords, with #\- in place of #\_. Most
  ;; GL constants ending in "_BIT" can be used without the
  ;; "-bit" in cl-opengl
  (gl:clear :color-buffer))

(defmethod draw :around ((w common1))
  ;; clear the colorbuffer.
  (clear w)

  ;; let subclasses decide what to actually draw
  (call-next-method)

  ;; swap the screen buffers
  (glop:swap-buffers w)
)


;; extension hooks used by subclasses
(defmethod init (w))

(defmethod cleanup (w))

;; the main loop is factored out so subclasses can use :around methods
;; to bind specials, etc around it
(defmethod main-loop (w)
  (init w)

  ;; game loop
  (loop
    ;; see if we are done
    until (window-should-close w)
    ;; check for events and send them to ON-FOO handlers. Returns
    ;; NIL when we should exit loop. We add a "continue" restart so
    ;; we can keep going if an even handler errors.
    while (with-simple-restart (continue "continue")
            ;; use the default ON-FOO handlers so we can dispatch on
            ;; specific keys if we want to
            (glop:dispatch-events w))
    do ;;; render
       (with-simple-restart (continue "continue")
         (draw w)))

  (cleanup w))

;; main entry point
(defun common (class)
  ;; glop:with-window creates a window and GL context with specified
  ;; parameters, and cleans up on exit
  (glop:with-window (w "LearnOpenGL" 800 600
                       ;; we ask for gl 3.3+ core profile
                       :major 3 :minor 3 :profile :core
                       ;; :WIN-CLASS class tells it to use the
                       ;; specified class for the window object
                       :win-class class)

    ;; cl-opengl handles loading the function pointers, so no GLEW

    ;;define viewport dimensions
    (gl:viewport 0 0 (glop:window-width w) (glop:window-height w))

    (main-loop w)))

#++
(common 'common1)


(defun build-shader (stage entry-point &rest more-stages)
  (let ((shader-program (gl:create-program)))

;;; build and compile our shader program
    ;; for each stage specified, create a shader, compile it and add
    ;; to SHADER-PROGRAM
    (loop
      for (stage entry-point) on (list* stage entry-point more-stages) by #'cddr
      for shader = (gl:create-shader stage)
      ;; gl:shader-source accepts a string or list of strings. We use
      ;; 3bgl-shader to compile lispy code into glsl in a string
      do (format t "~%~%compiling ~s for stage ~s::~%" entry-point stage)
      do (gl:shader-source shader
                           (print
                            (3bgl-shaders:generate-stage stage entry-point)))
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


;;; version of common a shader program and vao, based on triangle2
(defclass common2 (common1)
  ((shader-program :initform nil :accessor shader-program)
   (vao :initform nil :accessor vao)
   (buffers :initform nil :accessor buffers)
   ;; set with :default-initargs in subclass
   (shader-parts :initarg :shader-parts :reader shader-parts)
   (vbo-data :initarg :vbo-data :reader vbo-data)
   (ebo-data :initarg :ebo-data :reader ebo-data)
   (attributes :initarg :attributes :reader attributes))
  (:default-initargs
   ;; by default just draw a rectangle
   :vbo-data '(0.5 0.5 0.0 ; Top Right
               0.5 -0.5 0.0 ; Bottom Right
               -0.5 -0.5 0.0 ; Bottom Left
               -0.5 0.5 0.0 ; Top Left
               )
   :ebo-data '(0 1 3 ; first triangle
               1 2 3 ; second triangle
               )
   :attributes '((0 3 3 0))))

(defun common2-build-vao (w &key (vbo-data (vbo-data w))
                              (ebo-data (ebo-data w)))
  ;; cl-opengl has shortcuts to create 1 of most objects where the GL
  ;; API creates N at a time
  (let ((vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer))
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
    (static-vectors:with-static-vector (sv (length vbo-data)
                                           ;; for now assuming all floats
                                           :element-type 'single-float)
      ;; copy the vertex data into the static-vector
      (replace sv vbo-data)
      ;; and send it to GL. Package is %gl since we are using the
      ;; low-level API
      (%gl:buffer-data :array-buffer
                       ;; we don't have a convenient 'sizeof', so need
                       ;; to calculate the number of octets we are
                       ;; passing to GL manually
                       (* (length sv)
                          (cffi:foreign-type-size :float))
                       ;; pass the pointer for the static-vector
                       ;; array to GL
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    (gl:bind-buffer :element-array-buffer ebo)
    (static-vectors:with-static-vector (sv (length ebo-data)
                                           :element-type '(unsigned-byte 32))
      (replace sv ebo-data)
      (%gl:buffer-data :element-array-buffer
                       (* (length sv)
                          (cffi:foreign-type-size :unsigned-int))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    (loop with f = (cffi:foreign-type-size :float)
          for (index count stride offset) in (attributes w)
          do (gl:vertex-attrib-pointer index count :float nil
                                       (* stride f) (* offset f))
             (gl:enable-vertex-attrib-array index))

    ;; Note that this is allowed, the call to glVertexAttribPointer
    ;; registered VBO as the currently bound vertex buffer object so
    ;; afterwards we can safely unbind
    (gl:bind-buffer :array-buffer 0)

    ;; keep track of the buffer objects so we can delete them later
    (push vbo (buffers w))
    (push ebo (buffers w))

    ;; Unbind VAO (it's always a good thing to unbind any
    ;; buffer/array to prevent strange bugs)
    (gl:bind-vertex-array 0)
    ;; return the vao
    vao))

(defmethod init ((w common2))
  (setf (shader-program w) (apply 'build-shader (shader-parts w)))
  (setf (vao w) (common2-build-vao w)))

(defmethod cleanup ((w common2))
  (gl:delete-buffers (buffers w))
  (gl:delete-vertex-arrays (list (vao w)))
  (gl:delete-program (shader-program w)))

(defmethod prepare-draw ((w common2))
  ;; factor out initialization so subclasses can replace it if needed
  (gl:use-program (shader-program w))
  (gl:bind-vertex-array (vao w)))

(defmethod draw :around ((w common2))
  ;; set up for drawing
  (prepare-draw w)
  ;; do actual drawing
  (call-next-method)
  ;; clean up
  (gl:bind-vertex-array 0)
  (gl:use-program 0))

(defmethod draw ((w common2))
  (%gl:draw-elements :triangles (length (ebo-data w)) :unsigned-int 0))

#++ (common 'common2)

;; version of common2 with multiple-shaders

(defclass common3 (common2 camera-helper)
  ((clear-color :accessor clear-color
                :initform '(0.2 0.3 0.3 1) :initarg :clear-color)))

(defmethod init ((w common3))
  (setf (shader-program w) (make-hash-table))
  (loop for (name parts) on (shader-parts w) by #'cddr
        do (setf (gethash name (shader-program w))
                 (apply 'build-shader parts)))
  (setf (vao w) (common2-build-vao w))
  (gl:enable :depth-test))

(defmethod clear ((w common3))
  (apply #'gl:clear-color (clear-color w))
  (gl:clear :color-buffer :depth-buffer))

(defmethod cleanup ((w common3))
  (gl:delete-buffers (buffers w))
  (gl:delete-vertex-arrays (list (vao w)))
  (mapcar 'gl:delete-program
          (alexandria:hash-table-values (shader-program w))))

(defmethod get-program ((w common3) name)
  (gethash name (shader-program w)))

(defmethod prepare-draw ((w common3))
  (gl:bind-vertex-array (vao w)))

(defmethod draw ((w common3))
  (%gl:draw-elements :triangles (length (ebo-data w)) :unsigned-int 0))


;;; mixin for loading a set of textures

(defclass texture-mixin ()
  ((texture-ids :initform (make-hash-table) :reader texture-ids)
   (textures :initarg :textures :reader textures)))

(defun load-texture (filename &key (wrap-s :repeat) (wrap-t :repeat)
                                (min :linear-mipmap-linear)
                                (mag :linear))
  (let ((tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-min-filter min)
    (gl:tex-parameter :texture-2d :texture-mag-filter mag)
    (format t "loading texture ~s~%" filename)
    (let ((image #++(opticl:read-image-file
                  (or (probe-file
                       (asdf:system-relative-pathname :learnopengl filename))
                      filename))))
      (time (setf image
                  (opticl:read-image-file
              (or (probe-file
                   (asdf:system-relative-pathname :learnopengl filename))
                  filename))))
      ;; opticl loads images into a 3d array. to send it to GL, we
      ;; want a flat vector, ideally in form of a c-style pointer we
      ;; can pass directly to GL.
      (format t "uploading ~s~%" filename)
      (time
       (with-image-in-unsigned-bytes (image p format w h) ;; from utils.lisp
         (gl:tex-image-2d :texture-2d 0 :rgb w h 0 format :unsigned-byte p)
         (gl:generate-mipmap :texture-2d))))
    (gl:bind-texture :texture-2d 0)
    (format t "done~%")
    tex))

(defmethod init :after ((w texture-mixin))
  (loop for (key file . options) in (textures w)
        do (setf (gethash key (texture-ids w))
                 (apply #'load-texture file options))))

(defmethod bind-texture ((w texture-mixin) key &key (unit :texture0)
                                            (target :texture-2d))
  (let ((id (gethash key (texture-ids w) 0)))
    (format t "bind ~s =~s to ~s~%" key id unit)
    (gl:active-texture unit)
    (gl:bind-texture target id)))
