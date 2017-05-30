;;;; shader code
(defpackage textures-combined/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package textures-combined/shaders)

(input position :vec3 :location 0)
;; if we want to use the same name for different things in different
;; stages, we need to specify which we mean. If a stage isn't
;; specified, the same definition will be included in any stage that uses
;; the variable.
(input color :vec3 :location 1 :stage :vertex)
(input tex-coord :vec2 :location 2)
(output our-color :vec3 :stage :vertex)
(output -tex-coord :vec2 :stage :vertex)

(input our-color :vec3 :stage :fragment)
(input -tex-coord :vec2 :stage :fragment)
(output color :vec4 :stage :fragment)

(uniform our-texture :sampler-2d)
(uniform our-texture2 :sampler-2d)


(defun vertex ()
  (declare (stage :vertex))
  (setf gl-position (vec4 position 1)
        our-color color
        -tex-coord tex-coord))

(defun fragment ()
  (declare (stage :fragment))
  (setf color (mix (texture our-texture -tex-coord)
                   (texture our-texture2 -tex-coord)
                   0.2)))

;;;; back to normal lisp code
(in-package learning-opengl)

(defclass textures-combined (common2)
  ((texture1 :initform nil :accessor texture1)
   (texture2 :initform nil :accessor texture2))
  (:default-initargs
   :shader-parts '(:vertex-shader textures-combined/shaders::vertex
                   :fragment-shader textures-combined/shaders::fragment)
   :vbo-data
   ;; Positions     ;; Colors   ;; Texture Coords
   '(0.5  0.5 0.0   1.0 0.0 0.0   1.0 1.0   ; Top Right
     0.5 -0.5 0.0   0.0 1.0 0.0   1.0 0.0   ; Bottom Right
     -0.5 -0.5 0.0   0.0 0.0 1.0   0.0 0.0  ; Bottom Left
     -0.5  0.5 0.0   1.0 1.0 0.0   0.0 1.0) ; Top Left
   :ebo-data '(0 1 3
               1 2 3)
   :attributes '((0 3 8 0)
                 (1 3 8 3)
                 (2 2 8 6))))

(defmethod init :after ((w textures-combined))
  ;; load and create a texture
  (loop for file in (list "container.jpg" "awesomeface.png")
        for slot in '(texture1 texture2)
        do (let ((tex (gl:gen-texture)))
             (gl:bind-texture :texture-2d tex)

             ;; set the texture wrapping parameters
             (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
             (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
             ;; set texture filtering parameters
             (gl:tex-parameter :texture-2d :texture-min-filter :linear)
             (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
             ;; load image, create texture and generate mipmaps
             (let ((image (opticl:read-image-file
                           (asdf:system-relative-pathname
                            :learnopengl file))))
               ;; the macro stores the data in the order GL expects,
               ;; so we don't have an upside down texture. It
               ;; apparently doesn't handle alpha channel the same way
               ;; as SOIL though, so we end up with a white border on
               ;; the face image from the original code.
               (with-image-in-unsigned-bytes (image p format w h)
                 (gl:tex-image-2d :texture-2d 0 :rgb w h 0 format
                                  :unsigned-byte p)
                 (gl:generate-mipmap :texture-2d)))
             (gl:bind-texture :texture-2d 0)
             (setf (slot-value w slot) tex))))

(defmethod draw :before ((w textures-combined))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (texture1 w))
  ;; sometimes cl-opengl can't tell which version of the GL function
  ;; we want to call, so we still need the type suffix (it can usually
  ;; still tell how many arguments we passed though)
  (gl:uniformi (gl:get-uniform-location (shader-program w) "ourTexture1") 0)

  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d (texture2 w))
  (gl:uniformi (gl:get-uniform-location (shader-program w) "ourTexture2") 1))

#++
(common 'textures-combined)
