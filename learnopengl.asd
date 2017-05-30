(defsystem :learnopengl
  :depends-on (glop cl-opengl static-vectors 3bgl-shader opticl sb-cga mathkit)
  :serial t
  :components
  ((:module "factored"
    :components ((:file "package")
                 (:file "utils")
                 (:file "cube-vertices")
                 (:file "common")
                 (:file "triangle1")
                 (:file "triangle2")
                 (:file "shader-uniform")
                 (:file "shaders-interpolated")
                 (:file "textures")
                 (:file "textures-combined")
                 (:file "transformations")
                 (:file "coordinate-systems")
                 (:file "coordinate-systems-with-depth")
                 (:file "coordinate-systems-multiple-objects")
                 (:file "camera")
                 (:file "camera-zoom")
                 (:file "colors-scene")
                 (:file "basic-lighting-diffuse")
                 (:file "materials")))))