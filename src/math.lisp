(in-package #:mono)

(defconstant pi-f (coerce pi 'single-float) "Single float precision PI.")

(defun clamp (n lo hi)
  "Restricts the value of N to between LO and HI."
  (max (min n hi) lo))

(defun xyz-sphere* (x y z)
  "Converts cartesian coordinates X, Y and Z to spherical coordinates R, THETA
and PHI, where the up axis is the Y axis."
  (declare (type single-float x y z))
  (declare (values (simple-array single-float (3))))
  (let* ((r (sqrt (+ (* x x) (* y y) (* z z))))
         (theta (acos (/ y r)))
         (phi (atan z x)))
    (make-array 3 :initial-contents (vector r theta phi)
                  :adjustable nil
                  :element-type 'single-float)))

(defun xyz-sphere (vec)
  "Converts cartesian coordinates X, Y and Z to spherical coordinates R, THETA
and PHI, where the up axis is the Y axis."
  (declare (type (simple-vector 3) vec))
  (declare (values (simple-array single-float (3))))
  (xyz-sphere* (aref vec 0) (aref vec 1) (aref vec 2)))

(defun sphere-xyz* (r theta phi)
  "Converts spherical coordinates R, THETA and PHI to cartesian coordinates X, Y
and Z, where the up axis is the Y axis."
  (declare (type single-float r theta phi))
  (declare (values (simple-array single-float (3))))
  (let ((x (* r (cos phi) (sin theta)))
        (y (* r (cos theta)))
        (z (* r (sin phi) (sin theta))))
    (make-array 3 :initial-contents (vector x y z)
                  :adjustable nil
                  :element-type 'single-float)))

(defun sphere-xyz (vec)
  "Converts spherical coordinates R, THETA and PHI to cartesian coordinates X, Y
and Z, where the up axis is the Y axis."
  (declare (type (simple-vector 3) vec))
  (declare (values (simple-array single-float (3))))
  (sphere-xyz* (aref vec 0) (aref vec 1) (aref vec 2)))
