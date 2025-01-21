(defpackage :hello-triangle-color
  (:use :cl))

(in-package :hello-triangle-color)

(defparameter vbuf nil)
(defparameter cbuf nil)
(defparameter vao nil)

(defparameter vertices #(-0.5 -0.5 0.0
                         0.5 -0.5 0.0
                         0.0  0.5 0.0))

(defparameter colors #(1.0 0.0 0.0 1.0
                       0.0 1.0 0.0 1.0
                       0.0 0.0 1.0 1.0))

(when (string-equal "a" "") (print "empty"))


(niu:run-sketch (:fragment-shader niu:+fs-default+)

  ;; initialize buffers
  (setf vbuf (gl:gen-buffer))
  (setf cbuf (gl:gen-buffer))
  (setf vao (gl:gen-vertex-array))

  (gl:bind-vertex-array vao)

  ;; write vertices
  (niu:write-array-buffer vbuf vertices)
  (gl:vertex-attrib-pointer
   0 3 :float :false (niu:size-of :float 3) 0)
  (gl:enable-vertex-attrib-array 0)

  ;; write colors
  (niu:write-array-buffer cbuf colors)
  (gl:vertex-attrib-pointer
   1 4 :float :false (niu:size-of :float 4) 0)
  (gl:enable-vertex-attrib-array 1)

  (loop
    until (glfw:window-should-close-p)
    do (progn
         (gl:clear-color 0.0 0.0 0.0 1.0)
         (gl:clear :color-buffer-bit)
         (gl:draw-arrays :triangles 0 3)
         (glfw:swap-buffers)
         (glfw:poll-events))))
