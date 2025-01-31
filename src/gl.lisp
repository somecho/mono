(in-package #:mono)

(defun uniformf (name value)
  "Sets a uniform float to the current bound shader program."
  (declare (type (simple-array character) name)
           (type single-float value))
  (-> (gl:get-integer :current-program)
      (gl:get-uniform-location name)
      (gl:uniformf value)))

(defun uniform-mat4f (name mat)
  "Sets a uniform 4x4 float matrix to the current bound shader program."
  (declare (type (simple-array character) name)
           (type (simple-array single-float (16)) mat))
  (-> (gl:get-integer :current-program)
      (gl:get-uniform-location name)
      (gl:uniform-matrix-4fv mat)))
