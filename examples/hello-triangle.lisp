(defpackage :hello-triangle
  (:use :cl))

(in-package :hello-triangle)

(defparameter vertices #(-0.5 -0.5 0.0
                         0.5 -0.5 0.0
                         0.0  0.5 0.0))

(defparameter vbuf nil)
(defparameter vattribs nil)

(defun draw ()
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit)
  (gl:draw-arrays :triangles 0 3))

(niu:run-sketch ()
  (setf vbuf (gl:gen-buffer))
  (setf vattribs (gl:gen-vertex-array))
  (gl:bind-vertex-array vattribs)
  (niu:write-array-buffer vbuf vertices)
  (gl:vertex-attrib-pointer 0 3 :float :false (niu:size-of :float 3) 0)
  (gl:enable-vertex-attrib-array 0)
  (loop until (glfw:window-should-close-p)
        do (funcall 'draw)
        do (glfw:swap-buffers)
        do (glfw:poll-events)))
