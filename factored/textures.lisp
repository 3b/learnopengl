;;;; shader code
(defpackage textures/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package textures/shaders)

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


(defun vertex ()
  (declare (stage :vertex))
  (setf gl-position (vec4 position 1)
        our-color color
        -tex-coord tex-coord))

(defun fragment ()
  (declare (stage :fragment))
  (setf color (texture our-texture -tex-coord)))

(defun fragment2 ()
  (declare (stage :fragment))
  (setf color (* (texture our-texture -tex-coord)
                 (vec4 our-color 1.0))))

;;;; back to normal lisp code
(in-package learning-opengl)

(defclass textures (common2)
  ((texture :initform nil :accessor texture))
  (:default-initargs
   :shader-parts '(:vertex-shader textures/shaders::vertex
                   :fragment-shader textures/shaders::fragment)
   :vbo-data
   ;; Positions     ;; Colors   ;; Texture Coords
   '(0.5  0.5 0.0   1.0 0.0 0.0   1.0 1.0   ; Top Right
     0.5 -0.5 0.0   0.0 1.0 0.0   1.0 0.0   ; Bottom Right
     -0.5 -0.5 0.0   0.0 0.0 1.0   0.0 0.0  ; Bottom Left
     -0.5  0.5 0.0   1.0 1.0 0.0   0.0 1.0) ; Top Left

   :ebo-data '(0 1 3 1 2 3)
   :attributes '((0 3 8 0)
                 (1 3 8 3)
                 (2 2 8 6))))

(defmethod init :after ((w textures))
  ;; load and create a texture
  (let ((tex (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex)

;;; set the texture wrapping parameters
    ;; many functions in GL api have a suffix to denote type and
    ;; number of arguments. Wrappers in the GL: package of cl-opengl
    ;; generally figure that out for you.
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
;;; set texture filtering parameters
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
;;; load image, create texture and generate mipmaps
    (let ((image (opticl:read-image-file
                  (asdf:system-relative-pathname
                   :learnopengl "container.jpg"))))
      ;; opticl loads images into a 3d array. to send it to GL, we
      ;; want a flat vector, ideally in form of a c-style pointer we
      ;; can pass directly to GL.
      (with-image-in-unsigned-bytes (image p format w h) ;; from utils.lisp
        (gl:tex-image-2d :texture-2d 0 :rgb w h 0 format :unsigned-byte p)
        (gl:generate-mipmap :texture-2d)))
    (gl:bind-texture :texture-2d 0)
    (setf (texture w) tex)))

(defmethod draw :before ((w textures))
  (gl:bind-texture :texture-2d (texture w))
  )

#++
(common 'textures)
