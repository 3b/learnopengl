;;;; shader code
(defpackage materials/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package materials/shaders)

;; structures are defined with (defstruct <name> (slot-name slottype)*)
(defstruct -material
  (ambient :vec3)
  (diffuse :vec3)
  (specular :vec3)
  (shininess :float))

(defstruct -light
  (position :vec3)

  (ambient :vec3)
  (diffuse :vec3)
  (specular :vec3))

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
(uniform material -material)
(uniform light -light)
(uniform light-color :vec3)
(uniform view-pos :vec3)


(defun vertex ()
  (setf gl-position (* projection view model (vec4 position 1))
        -normal (* (inverse (transpose (mat3 model))) normal)
        frag-pos (vec3 (* model (vec4 position 1)))))

(defun fragment ()
  (let* ((ambient-strength 0.1)
         ;; structure slot access currently uses @ (or
         ;; slot-value)rather than CL style accessors. @ chains an
         ;; arbitrary # of slot accesses of nested structs
         (ambient (* (@ light ambient) (@ material ambient)))

         (normal (normalize -normal))
         (light-dir (normalize (- (@ light position) frag-pos)))
         (diff (max (dot normal light-dir) 0))
         (diffuse (* (@ light diffuse) diff (@ material diffuse)))

         (specular-strength 0.5)
         (view-dir (normalize (- view-pos frag-pos)))
         (reflect-dir (reflect (- light-dir) normal))
         (spec (pow (max (dot view-dir reflect-dir) 0) (@ material shininess)))
         (specular (* (@ light specular) spec (@ material specular)))

         (result (+ ambient diffuse specular))
)
   (setf color (vec4 result 1))))

(defun lamp-vertex ()
  (setf gl-position (* projection view model (vec4 position 1))))


(defun lamp-fragment ()
  (setf color (vec4 light-color 1)))

;;;; back to normal lisp code

(in-package learning-opengl)

(defclass materials (cube/uv common3)
  ;; light attributes
  ((light-pos :accessor light-pos :initform (sb-cga:vec 1.2 1.0 2.0)))
  (:default-initargs
   :shader-parts
   '(:lighting (:vertex-shader materials/shaders::vertex
                :fragment-shader materials/shaders::fragment)
     :lamp (:vertex-shader materials/shaders::lamp-vertex
            :fragment-shader materials/shaders::lamp-fragment))))

(defmethod draw :before ((w materials))
  (loop
    for shader in '(:lighting :lamp)
    for model in (list (sb-cga:identity-matrix)
                       (sb-cga:matrix* (sb-cga:translate (light-pos w))
                                       (sb-cga:scale* 0.2 0.2 0.2)))
    for program = (get-program w shader)
    for uniform-locs = (get-uniforms
                        program
                        '(model view-pos light-color
                          light.ambient light.diffuse light.specular
                          light.position
                          material.ambient material.diffuse material.specular
                          material.shininess))
    for time = (/ (get-internal-real-time)
                  (float internal-time-units-per-second))
    for light-color = (sb-cga:vec (sin (* time 2))
                                  (sin (* time 0.7))
                                  (sin (* time 1.3)))
    do (macrolet ((u (name)
                    `(gethash ',name uniform-locs)))
         (gl:use-program program)
         (gl:uniformf (u material.ambient) 1 0.5 0.31)
         (gl:uniformf (u material.diffuse) 1 0.5 0.31)
         (gl:uniformf (u material.specular) 0.5 0.5 0.5)
         (gl:uniformf (u material.shininess) 32)
         (gl:uniformfv (u light.ambient) (sb-cga:vec* light-color 0.2))
         (gl:uniformfv (u light.diffuse) (sb-cga:vec* light-color 0.5))
         (gl:uniformf (u light.specular) 1 1 1)
         (gl:uniformfv (u light.position) (light-pos w))
         (gl:uniformfv (u view-pos) (pos (current-camera w)))

         (gl:uniform-matrix-4fv (u model) model nil)
         (set-view-projection w program)
         ;; set light uniforms

         (%gl:draw-elements :triangles (length (ebo-data w)) :unsigned-int 0))))


#++
(common 'materials)
