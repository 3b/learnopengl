
(in-package learning-opengl)

(defclass model-loading (common3)
  ((light-pos :accessor light-pos
              :initform (list
                         (sb-cga:vec 0.7 0.2 2.0)
                         (sb-cga:vec 2.3 -3.3 -4.0)
                         (sb-cga:vec -4.0 2.0 -12.0)
                         (sb-cga:vec 0.0 0.0 -3.0)))
   (models :accessor models :initform nil))
  (:default-initargs
   :shader-parts
   '(:lighting (:vertex-shader multiple-lights/shaders::vertex
                :fragment-shader multiple-lights/shaders::fragment))))

(defmethod main-loop :around ((w model-loading))
  (with-texture-cache ()
    (call-next-method)))

(defmethod init :after ((w model-loading))
  ;; could also put this in main-loop :around method
  (setf (models w)
        (list (load-model (asdf:system-relative-pathname
                           'learnopengl
                           "resources/objects/nanosuit/nanosuit.obj")))))

(defmethod prepare-draw ((w model-loading)))

(defmethod draw ((w model-loading))
  (loop
    for model in (models w)
    for matrix = (sb-cga:identity-matrix)
    for program = (get-program w :lighting)
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
         (gl:uniform-matrix-4fv (u model) matrix nil)
         (draw-model model program))))


#++
(common 'model-loading)
