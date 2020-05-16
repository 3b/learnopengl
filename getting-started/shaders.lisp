(defpackage learning-opengl-shaders
  (:use #:3bgl-glsl/cl))
(in-package learning-opengl-shaders)

;; "in" variables are defined with (input name type &key location stage)
(input position :vec3 :location 0)
(input color :vec3 :location 1)
;; if we want to use the same name for different things in different
;; stages, we need to specify which we mean. If a stage isn't
;; specified, the definition will be included in any stage that uses
;; the variable.

;; "out" variables use (output ...)
(output our-color :vec3 :stage :vertex) ;; 
(output color :vec4 :stage :fragment)

;; uniforms allow more keywords:
;; (uniform name type &key stage location layout qualifiers default)
(uniform our-color :vec3 :stage :fragment)

;; we give normal names to the "main" function, so can put a bunch in
;; same file without having any conflicts
(defun shaders-uniform-vertex ()
  ;; don't need to specify types in functions
  (setf gl-position (vec4 position 1)
        our-color color))

(defun shaders-uniform-fragment ()
  (setf color))
