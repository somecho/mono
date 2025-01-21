(defpackage :hello-triangle
  (:use :arrows :cl))

(in-package :hello-triangle)


(defparameter vertices #(-0.5 -0.5 0.0
                         0.5 -0.5 0.0
                         0.0  0.5 0.0))
(defparameter vbuf nil)
(defparameter vattribs nil)

(defun draw ())

(niu:run-sketch ()
  (setf vbuf (gl:gen-buffer))
  (setf vattribs (gl:gen-vertex-array))
  (loop until (glfw:window-should-close-p)
        do (funcall 'draw)
        do (glfw:swap-buffers)
        do (glfw:poll-events)))
