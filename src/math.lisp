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
  (declare (type (or (simple-vector 3) (simple-array single-float (3))) vec))
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
  (declare (type (or (simple-vector 3) (simple-array single-float (3))) vec))
  (declare (values (simple-array single-float (3))))
  (sphere-xyz* (aref vec 0) (aref vec 1) (aref vec 2)))

(defun sample-tri* (a b c)
  "Uniformly samples triangle with vertices A B C."
  (declare (type (simple-array single-float (3)) a b c))
  (let* ((u (/ (random 1001) 1000.0))
         (v (/ (random 1001) 1000.0)))
    (when (> (+ u v) 1.0)
      (setf u (- 1.0 u))
      (setf v (- 1.0 v)))
    (let ((a1 (kit.glm:vec* a (- 1.0 u v)))
          (b1 (kit.glm:vec* b u))
          (c1 (kit.glm:vec* c v)))
      (kit.glm:vec+ (kit.glm:vec+ a1 b1) c1))))

(defun sample-tri (tri)
  "Uniformly samples triangle TRI."
  (declare (type (or (vector single-float 9)
                     (simple-vector 9)
                     (simple-array single-float (9))) tri)
           (values (simple-array single-float (3))))
  (let ((a (make-array 3 :element-type 'single-float :initial-contents (subseq tri 0 3)))
        (b (make-array 3 :element-type 'single-float :initial-contents (subseq tri 3 6)))
        (c (make-array 3 :element-type 'single-float :initial-contents (subseq tri 6 9))))
    (sample-tri* a b c)))

(defun sample-tri-strip (tri-strip)
  "Uniformly generate a pseudorandom point within TRI-STRIP, which must be a flat
vector of 3D vertices."
  (declare (type vector tri-strip)
           (values (vector single-float 3)))
  (let* ((num-vertices (-> tri-strip length (/ 3)))
         (index (* 3 (random (- num-vertices 2))))
         (tri (make-array 9 :element-type 'single-float
                            :initial-contents (subseq tri-strip index (+ index 9)))))
    (mono:sample-tri tri)))

(defun sample-tri-fan (tri-fan)
  "Uniformly generate a pseudorandom point within TRI-FAN, which must be a flat
vector of 3D vertices."
  (declare (type vector tri-fan)
           (values (vector single-float 3)))
  (let* ((num-vertices (-> tri-fan length (/ 3)))
         (index (* 3 (+ 1 (random (- num-vertices 3)))))
         (a (make-array 3 :element-type 'single-float
                          :initial-contents (subseq tri-fan 0 3)))
         (b (make-array 3 :element-type 'single-float
                          :initial-contents (subseq tri-fan index (+ index 3))))
         (c (make-array 3 :element-type 'single-float
                          :initial-contents (subseq tri-fan (+ index 3) (+ index 6)))))
    (mono:sample-tri* a b c)))
