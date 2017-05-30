;;;; shader code
(defpackage colors-scene/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package colors-scene/shaders)

(input position :vec3 :location 0)
(uniform model :mat4)
(uniform view :mat4)
(uniform projection :mat4)

(output color :vec4 :stage :fragment)
(uniform object-color :vec3)
(uniform light-color :vec3)
(defun vertex ()
  (setf gl-position (* projection view model (vec4 position 1))))

(defun fragment ()
  (setf color (vec4 (* light-color object-color) 1)))

(defun lamp-vertex ()
  (setf gl-position (* projection view model (vec4 position 1))))

(defun lamp-fragment ()
  (setf color (vec4 light-color 1)))

;;;; back to normal lisp code

(in-package learning-opengl)

(defclass colors-scene (cube/uv common3)
  ;; light attributes
  ((light-pos :accessor light-pos :initform (sb-cga:vec 1.2 1.0 2.0)))
  (:default-initargs
   :shader-parts
   '(:lighting (:vertex-shader colors-scene/shaders::vertex
                :fragment-shader colors-scene/shaders::fragment)
     :lamp (:vertex-shader colors-scene/shaders::lamp-vertex
            :fragment-shader colors-scene/shaders::lamp-fragment))))

(defmethod draw :before ((w colors-scene))
  (loop
    for shader in '(:lighting :lamp)
    for model in (list (sb-cga:identity-matrix)
                       (sb-cga:matrix* (sb-cga:translate (light-pos w))
                                       (sb-cga:scale* 0.2 0.2 0.2)))
    for program = (get-program w shader)
    ;; just using symbols for their names here, so don't need to
    ;; use the actual symbols from the shader package.
    for uniform-locs = (get-uniforms program
                                     '(object-color light-color model))
    do (macrolet ((u (name)
                    `(gethash ',name uniform-locs)))
         ;; use the corresponding shader when setting uniforms/drawing objects
         (gl:use-program program)
         (gl:uniformf (u object-color) 1 0.5 0)
         (gl:uniformf (u light-color) 1 0.5 1)

         (gl:uniform-matrix-4fv (u model) model nil)
         (set-view-projection w program)
         (%gl:draw-elements :triangles (length (ebo-data w)) :unsigned-int 0))))


#++
(common 'colors-scene)
