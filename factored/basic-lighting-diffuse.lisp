;;;; shader code
(defpackage basic-lighting-diffuse/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package basic-lighting-diffuse/shaders)

(input position :vec3 :location 0)
(input normal :vec3 :location 1)
(output -normal :vec3 :stage :vertex)
(output frag-pos :vec3 :stage :vertex)

(uniform model :mat4)
(uniform view :mat4)
(uniform projection :mat4)

(output color :vec4 :stage :fragment)
(input -normal :vec3 :stage :fragment)
(input frag-pos :vec3 :stage :fragment)
(uniform object-color :vec3)
(uniform light-color :vec3)
(uniform light-pos :vec3)
(uniform view-pos :vec3)
(defun vertex ()
  (setf gl-position (* projection view model (vec4 position 1))
        -normal (* (inverse (transpose (mat3 model))) normal)
        frag-pos (vec3 (* model (vec4 position 1)))))

(defun fragment ()
  (let* ((ambient-strength 0.1)
         (ambient (* ambient-strength light-color))

         (normal (normalize -normal))
         (light-dir (normalize (- light-pos frag-pos)))
         (diff (max (dot normal light-dir) 0))
         (diffuse (* diff light-color))

         (specular-strength 0.5)
         (view-dir (normalize (- view-pos frag-pos)))
         (reflect-dir (reflect (- light-dir) normal))
         (spec (pow (max (dot view-dir reflect-dir) 0) 32))
         (specular (* specular-strength spec light-color))

         (result (* (+ ambient diffuse specular) object-color))
)
   (setf color (vec4 result 1))))

(defun lamp-vertex ()
  (setf gl-position (* projection view model (vec4 position 1))))

(defun lamp-fragment ()
  (setf color (vec4 light-color 1)))

;;;; back to normal lisp code

(in-package learning-opengl)

(defclass basic-lighting-diffuse (cube/uv common3)
  ;; light attributes
  ((light-pos :accessor light-pos :initform (sb-cga:vec 1.2 1.0 2.0)))
  (:default-initargs
   :shader-parts
   '(:lighting (:vertex-shader basic-lighting-diffuse/shaders::vertex
                :fragment-shader basic-lighting-diffuse/shaders::fragment)
     :lamp (:vertex-shader basic-lighting-diffuse/shaders::lamp-vertex
            :fragment-shader basic-lighting-diffuse/shaders::lamp-fragment))))

(defmethod draw :before ((w basic-lighting-diffuse))
  (loop
    for shader in '(:lighting :lamp)
    for model in (list (sb-cga:identity-matrix)
                       (sb-cga:matrix* (sb-cga:translate (light-pos w))
                                       (sb-cga:scale* 0.2 0.2 0.2)))
    for program = (get-program w shader)
    for uniform-locs = (get-uniforms program '(object-color light-color model
                                               light-pos view-pos))
    do (macrolet ((u (name)
                    `(gethash ',name uniform-locs)))
         (gl:use-program program)
         (gl:uniformf (u object-color) 1 0.5 0)
         (gl:uniformf (u light-color) 1 0.5 1)
         (gl:uniformfv (u light-pos) (light-pos w))
         (gl:uniformfv (u view-pos) (pos (current-camera w)))

         (gl:uniform-matrix-4fv (u model) model nil)
         (set-view-projection w program)
         ;; set light uniforms

         (%gl:draw-elements :triangles (length (ebo-data w)) :unsigned-int 0))))


#++
(common 'basic-lighting-diffuse)

