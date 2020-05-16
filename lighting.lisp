(cl:defpackage learning-opengl-shaders
  (:use :3bgl-glsl/cl))
(cl:in-package learning-opengl-shaders)

(interface varyings (:out (:vertex t)
                     :in (:fragment t))
  (tex-coords :vec2)
  (world-pos :vec3)
  (normal :vec3))

(output frag-color :vec4)

(uniform albedo :vec3)
(uniform metallic :float)
(uniform roughness :float)
(uniform ao :float)

(uniform light-positions (:vec3 4))
(uniform light-colors (:vec3 4))

(uniform cam-pos :vec3)

(defconstant fpi #.(float pi 1.0) :float)


(defun distribution-ggx (n h roughness)
  (let* ((a (* roughness roughness))
         (a2 (* a a))
         (n.h (max (dot n h) 0))
         (n.h2 (* n.h n.h))
         (num a2)
         (denom (+ (* n.h2 (- a2 1)) 1))
         (denom (* fpi denom denom)))
    (return (/ num denom))))

(defun geometry-schlick-ggx (n.v roughness)
  (let* ((r (1+ roughness))
         (k (/ (* r r) 8))
         (num n.v)
         (denom (+ (* n.v (- 1 k)) k)))
    (return (/ num denom))))

(defun geometry-smith (n v l roughness)
  (let* ((n.v (max (dot n v) 0))
         (n.l (max (dot n l) 0))
         (ggx2 (geometry-schlick-ggx n.v roughness))
         (ggx1 (geometry-schlick-ggx n.l roughness)))
    (return (* ggx1 ggx2))))

(defun fresnel-schlick (cos-theta f0)
  (return (+ f0
             (* (- 1 f0)
                (expt (- 1 cos-theta) 5)))))


(defun fragment ()
  (let ((n (normalize normal))
        (v (normalize (- cam-pos world-pos)))
        (lo (vec3 0))
        (f0 (vec3 0.04)))
    (setf f0 (mix f0 albedo metallic))
    (dotimes (i 4)
      (let* ((l (normalize (- (aref light-positions i)
                              world-pos)))
             (h (normalize (+ v l)))
             (distance (length (- (aref light-positions i)
                                  world-pos)))
             (attenuation (/ 1 (* distance distance)))
             (radiance (* (aref light-colors i)
                          attenuation))

             (ndf (distribution-ggx n h roughness))
             (g (geometry-smith n v l roughness))
             (f (fresnel-schlick (max (dot h v) 0) f0))

             (ks f)
             (kd (- 1 ks))
             (kd (* kd (- 1 metallic)))

             (numerator (* ndf g f))
             (denominator (+ (* 4
                                (max (dot n v) 0)
                                (max (dot n l) 0))
                             0.001))
             (specular (/ numerator denominator))

             (n.l (max (dot n l) 0)))
        (incf lo (* (+ (/ (* kd albedo)
                          fpi)
                       specular)
                    radiance
                    n.l))))
    (let* ((ambient (* (vec3 0.03) albedo ao))
           (color (+ ambient lo)))
      (setf color (/ color (+ color 1)))
      (setf color (pow color (vec3 (/ 1 2.2))))
      (setf frag-color (vec4 color 1)))))
