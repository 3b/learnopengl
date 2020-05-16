(in-package learning-opengl)

(defclass model ()
  ((meshes :accessor meshes :initform nil)
   (base-directory :accessor base-directory :initarg :base-directory)))

(defmethod draw-model ((m model) shader)
  (loop for mesh in (meshes m)
        do (draw-mesh mesh shader)))

;;; we use classimp CL library to talk to the assimp C library
(defun load-model (path)
  ;; put output from assimp somewhere we can (hopefully) see it
  (ai:with-log-to-stdout ()
    ;; and load the file
    (let ((scene (ai:import-into-lisp
                  ;; processing flags are specified as a list of
                  ;; keywords rather than c-style enums combined with |
                  path :processing-flags '(:ai-process-triangulate
                                           :ai-process-flip-u-vs)))
          (m (make-instance 'model)))
      (unless (and scene
                   (not (ai:scene-incomplete-p scene))
                   (ai:root-node scene))
        (format t "ERROR::ASSIMP::~a~%" (%ai:ai-get-error-string)))
      (setf (base-directory m)
            (uiop:pathname-directory-pathname path))
      (process-node m (ai:root-node scene) scene)
      (setf (meshes m) (reverse (meshes m)))
      m)))

(defmethod process-node ((m model) node scene)
  (loop for mesh-index across (ai:meshes node)
        for mesh = (aref (ai:meshes scene) mesh-index)
        do (push (process-mesh m mesh scene) (meshes m)))
  (loop for child across (ai:children node)
        do (process-node m child scene)))

(defun process-mesh (model mesh scene)
  (make-instance
   'mesh
   :vertices
   (loop for i below (length (ai:vertices mesh))
         ;; classimp returns vertex/normal/(bi)tangent data as
         ;; (simple-array 'single-float (3)), so we can use that directly
         for vertex = (aref (ai:vertices mesh) i)
         for normal = (aref (ai:normals mesh) i)
         for tangent = (when (ai:tangents mesh)
                         (aref (ai:tangents mesh) i))
         for bitangent = (when (ai:bitangents mesh)
                           (aref (ai:bitangents mesh) i))
         ;; texcoord is same format, but we only pass 2 values to GL,
         ;; so get rid of the extra here
         for tex-coord = (unless (zerop (length (ai:texture-coords mesh)))
                           (subseq
                            (aref (aref (ai:texture-coords mesh) 0) i)
                            0 2))
         collect (list vertex normal tex-coord tangent bitangent))
   :indices
   (loop for face across (ai:faces mesh)
         collect (aref face 0)
         collect (aref face 1)
         collect (aref face 2))
   :textures
   (when (plusp (ai:material-index mesh))
     (let ((material (aref (ai:materials scene) (ai:material-index mesh))))
       (append
        (load-material-textures model material :ai-texture-type-diffuse
                                :texture_diffuse)
        (load-material-textures model material :ai-texture-type-specular
                                :texture_specular))))))

(defun load-material-textures (model mat type type-name)
  ;; classimp doesn't currently have an api like
  ;; GetTextureCount(type)/GetTexture(type,i,&str), so do it
  ;; manually...
  (loop with files = (gethash "$tex.file" mat)
        ;; type, channel, name
        for (nil nil file) in (remove type files :key 'car :test-not 'eql)
        collect (make-instance 'texture
                               :id (load-texture-cached
                                    (merge-pathnames
                                     file (base-directory model)))
                               :type type-name)))
