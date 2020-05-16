;;;; shader code
(defpackage multiple-lights/shaders
  (:use #:3bgl-glsl/cl)
  ;; shadow POSITION so we don't conflict with the default definition
  ;; of CL:POSITION as (input position :vec4 :location 0)
  (:shadow position))
(in-package multiple-lights/shaders)

;; structures are defined with (defstruct <name> (slot-name slottype)*)
(defstruct -material
  (diffuse :sampler-2d)
  (specular :sampler-2d)
  (shininess :float))

(defstruct -dir-light
  (direction :vec3)

  (ambient :vec3)
  (diffuse :vec3)
  (specular :vec3))

(defstruct -point-light
  (position :vec3)

  (constant :float)
  (linear :float)
  (quadratic :float)

  (ambient :vec3)
  (diffuse :vec3)
  (specular :vec3))

(defstruct -spot-light
  (position :vec3)

  (constant :float)
  (linear :float)
  (quadratic :float)

  ;; ...

  (ambient :vec3)
  (diffuse :vec3)
  (specular :vec3))



(input position :vec3 :location 0)
(input normal :vec3 :location 1)
(input tex-coords :vec2 :location 2)
(output -normal :vec3 :stage :vertex)
(output frag-pos :vec3 :stage :vertex)
(output -tex-coords :vec2 :stage :vertex)

(uniform model :mat4)
(uniform view :mat4)
(uniform projection :mat4)

(output color :vec4 :stage :fragment)
(input -normal :vec3 :stage :fragment)
(input frag-pos :vec3 :stage :fragment)
(input -tex-coords :vec2 :stage :fragment)
(uniform material -material)
(uniform dir-light -dir-light)
(defconstant +nr-point-lights+ 4 :int)
(uniform point-lights (-point-light +nr-point-lights+))
(uniform spot-light -spot-light)
(uniform light-color :vec3)
(uniform view-pos :vec3)

(defun calc-dir-light (light normal view-dir)
  ;; type inference can't figure out struct types by itself, so have
  ;; to specify those manually
  (declare (type -dir-light light))
 (let* ((light-dir (normalize (- (@ light direction))))
         ;; diffuse shading
         (diff (max (dot normal light-dir) 0.0))
         ;; specular shading
         (reflect-dir (reflect (- light-dir) normal))
         (spec (pow (max (dot view-dir reflect-dir) 0.0)
                    (@ material shininess)))
         ;; combine results
         (ambient (* (@ light ambient)
                     (vec3 (texture (@ material diffuse) -tex-coords))))
         (diffuse (* (@ light diffuse)
                     diff
                     (vec3 (texture (@ material diffuse) -tex-coords))))
         (specular (* (@ light specular)
                      spec
                      (vec3 (texture (@ material specular) -tex-coords)))))
    ;; 3bgl-glsl requires explicit RETURN form
    (return (+ ambient diffuse specular))))

(defun calc-point-light (light normal frag-pos view-dir)
  (declare (type -point-light light))
  (let* ((light-dir (normalize (- (@ light position) frag-pos)))
         ;; diffuse shading
         (diff (max (dot normal light-dir) 0.0))
         ;; specular shading
         (reflect-dir (reflect (- light-dir) normal))
         (spec (pow (max (dot view-dir reflect-dir) 0.0)
                    (@ material shininess)))
         ;; attenuation
         (distance (length (- (@ light position) frag-pos)))
         (attenuation (/ 1 (+ (@ light constant)
                              (* (@ light linear) distance)
                              (* (@ light quadratic) (* distance distance)))))
         ;; combine results
         (ambient (* (@ light ambient)
                     (vec3 (texture (@ material diffuse) -tex-coords))
                     attenuation))
         (diffuse (* (@ light diffuse)
                     diff
                     (vec3 (texture (@ material diffuse) -tex-coords))
                     attenuation))
         (specular (* (@ light specular)
                      spec
                      (vec3 (texture (@ material specular) -tex-coords)))
                   attenuation))
    ;; 3bgl-glsl requires explicit RETURN form
    (return (+ ambient diffuse specular))))

(defun vertex ()
  (setf gl-position (* projection view model (vec4 position 1))
        -normal (* (inverse (transpose (mat3 model))) normal)
        frag-pos (vec3 (* model (vec4 position 1)))
        -tex-coords tex-coords))

(defun fragment ()
  (let ((result (vec3 0 0 0))
        (norm (normalize -normal))
        (view-dir (normalize (- view-pos frag-pos))))
    (incf result (calc-dir-light dir-light norm view-dir))
    (dotimes (i +nr-point-lights+)
      (incf result (calc-point-light (aref point-lights i)
                                     norm frag-pos view-dir)))
    ;;(incf result (calc-spot-light))
    (setf color (vec4 result 1))
    ;;(setf color (vec4 (calc-dir-light dir-light norm view-dir) 1))
    #++(setf color (vec4 (calc-point-light (aref point-lights 0)
                                        norm frag-pos view-dir)
                      1))
    ))

(defun lamp-vertex ()
  (setf gl-position (* projection view model (vec4 position 1))))

(defun lamp-fragment ()
  (setf color (vec4 light-color 1)))

;;;; back to normal lisp code

(in-package learning-opengl)

(defclass multiple-lights (cube/uv texture-mixin common3)
  ((light-pos :accessor light-pos
              :initform (list
                         (sb-cga:vec 0.7 0.2 2.0)
                         (sb-cga:vec 2.3 -3.3 -4.0)
                         (sb-cga:vec -4.0 2.0 -12.0)
                         (sb-cga:vec 0.0 0.0 -3.0)))
   (cube-positions :accessor cube-positions
                   :initform (list
                              (sb-cga:vec  0.0 0.0 0.0)
                              (sb-cga:vec  2.0 5.0 -15.0)
                              (sb-cga:vec -1.5 -2.2 -2.5)
                              (sb-cga:vec -3.8 -2.0 -12.3)
                              (sb-cga:vec  2.4 -0.4 -3.5)
                              (sb-cga:vec -1.7 3.0 -7.5)
                              (sb-cga:vec  1.3 -2.0 -2.5)
                              (sb-cga:vec  1.5 2.0 -2.5)
                              (sb-cga:vec  1.5 0.2 -1.5)
                              (sb-cga:vec -1.3 1.0 -1.5))))
  (:default-initargs
   :shader-parts
   '(:lighting (:vertex-shader multiple-lights/shaders::vertex
                :fragment-shader multiple-lights/shaders::fragment)
     :lamp (:vertex-shader multiple-lights/shaders::lamp-vertex
            :fragment-shader multiple-lights/shaders::lamp-fragment))
   :textures '((:container2 "container2.png")
               (:container2-specular "container2_specular.png"))))

(defmethod draw ((w multiple-lights))
  (bind-texture w :container2)
  (bind-texture w :container2-specular :unit :texture1)
  (loop
    for shader in '(:lighting :lamp)
    for model in (list (sb-cga:identity-matrix)
                       (sb-cga:matrix* (sb-cga:identity-matrix)
                                       (sb-cga:scale* 0.2 0.2 0.2)))
    for program = (get-program w shader)
    do (macrolet ((u (name)
                    `(get-uniform program ',name)))
         (gl:use-program program)

         (gl:uniformf (u dir-light.direction) -0.2 -1 -0.3)
         (gl:uniformf (u dir-light.ambient) 0.05 0.05 0.05)
         (gl:uniformf (u dir-light.diffuse) 0.4 0.4 0.4)
         (gl:uniformf (u dir-light.specular) 0.5 0.5 0.5)

         (gl:uniformfv (u point-lights[0].position) (elt (light-pos w) 0))
         (gl:uniformf (u point-lights[0].ambient) 0.05 0.05 0.05)
         (gl:uniformf (u point-lights[0].diffuse) 0.8 0.8 0.8)
         (gl:uniformf (u point-lights[0].specular) 1 1 1)
         (gl:uniformf (u point-lights[0].constant) 1)
         (gl:uniformf (u point-lights[0].linear) 0.09)
         (gl:uniformf (u point-lights[0].quadratic) 0.032)

         (gl:uniformfv (u point-lights[1].position) (elt (light-pos w) 1))
         (gl:uniformf (u point-lights[1].ambient) 0.05 0.05 0.05)
         (gl:uniformf (u point-lights[1].diffuse) 0.8 0.8 0.8)
         (gl:uniformf (u point-lights[1].specular) 1 1 1)
         (gl:uniformf (u point-lights[1].constant) 1)
         (gl:uniformf (u point-lights[1].linear) 0.09)
         (gl:uniformf (u point-lights[1].quadratic) 0.032)

         (gl:uniformfv (u point-lights[2].position) (elt (light-pos w) 2))
         (gl:uniformf (u point-lights[2].ambient) 0.05 0.05 0.05)
         (gl:uniformf (u point-lights[2].diffuse) 0.8 0.8 0.8)
         (gl:uniformf (u point-lights[2].specular) 1 1 1)
         (gl:uniformf (u point-lights[2].constant) 1)
         (gl:uniformf (u point-lights[2].linear) 0.09)
         (gl:uniformf (u point-lights[2].quadratic) 0.032)

         (gl:uniformfv (u point-lights[3].position) (elt (light-pos w) 3))
         (gl:uniformf (u point-lights[3].ambient) 0.05 0.05 0.05)
         (gl:uniformf (u point-lights[3].diffuse) 0.8 0.8 0.8)
         (gl:uniformf (u point-lights[3].specular) 1 1 1)
         (gl:uniformf (u point-lights[3].constant) 1)
         (gl:uniformf (u point-lights[3].linear) 0.09)
         (gl:uniformf (u point-lights[3].quadratic) 0.032)




         (gl:uniformf (u material.ambient) 1 0.5 0.31)
         (gl:uniformi (u material.diffuse) 0)
         (gl:uniformi (u material.specular) 1)
         (gl:uniformf (u material.shininess) 32)



         (gl:uniformfv (u view-pos) (pos (current-camera w)))

         (set-view-projection w program)
         ;; set light uniforms
         (if (eq shader :lighting)
             (loop for p in (cube-positions w)
                   for model = (sb-cga:translate p)
                   do (gl:uniform-matrix-4fv (u model) model nil)
                      (%gl:draw-elements :triangles (length (ebo-data w))
                                         :unsigned-int 0))
             (progn
               (gl:uniform-matrix-4fv (u model) model nil)
               (%gl:draw-elements :triangles (length (ebo-data w))
                                  :unsigned-int 0))))))


#++
(common 'multiple-lights)
