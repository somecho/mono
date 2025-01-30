(in-package #:mono)

(defclass camera ()
  ((pos
    :initform (kit.glm:vec3 1.0 1.0 1.0)
    :initarg :pos
    :accessor pos)
   (view-origin
    :initform (kit.glm:vec3 0.0)
    :initarg :view-origin
    :accessor view-origin)
   (up-vector
    :initform (kit.glm:vec3 0.0 1.0 0.0)
    :initarg :up
    :accessor up-vector)
   (view-mat :initform nil :accessor view-mat)
   (p-mouse :initform nil :accessor p-mouse)))

(defmethod initialize-instance :after ((cam camera) &key)
  (with-slots (pos view-origin up-vector view-mat) cam
    (setf view-mat (kit.glm:look-at pos view-origin up-vector))))

(defmethod update ((cam camera))
  (with-slots (p-mouse pos view-mat view-origin up-vector) cam
    (let ((mousepressed (eql :PRESS (glfw:get-mouse-button 0))))
      (when mousepressed
        (let* ((f 0.005)
               (lo 0.001)
               (mouse-pos (glfw:get-cursor-position))
               (x (-> mouse-pos first (coerce 'single-float)))
               (y (-> mouse-pos second (coerce 'single-float)))
               (dx (->> p-mouse first (- x)))
               (dy (->> p-mouse second (- y)))
               (hi (- mono:pi-f lo))
               (sc (mono:xyz-sphere pos))
               (rotated (mono:sphere-xyz* (aref sc 0)
                                          (-> (aref sc 1) (- (* f dy)) (mono:clamp lo hi))
                                          (-> (aref sc 2) (+ (* f dx))))))
          (setf pos (kit.glm:vec3 rotated))
          (setf view-mat (kit.glm:look-at pos view-origin up-vector))))
      (setf p-mouse (mapcar (lambda (x) (coerce x 'single-float))
                            (glfw:get-cursor-position))))))
