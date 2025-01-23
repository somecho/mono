(defpackage #:hello-quad (:use :cl))
(in-package #:hello-quad)

;; Showcases use of indices

(defparameter vertices  #(0.5  0.5 0.0    ;; top right
                          0.5 -0.5 0.0    ;; bottom right
                          -0.5 -0.5 0.0   ;; bottom left
                          -0.5 0.5 0.0))  ;; top left

(defparameter indices #(0 1 3 1 2 3))

(defparameter vbo nil)
(defparameter vao nil)

(niu:run-sketch (:width 600 :height 600)
  (setf vbo (gl:gen-buffer))
  (setf vao (gl:gen-vertex-array))
  (gl:bind-vertex-array vao)
  (niu:write-array-buffer vbo (niu:gl-array vertices))
  (gl:vertex-attrib-pointer 0 3 :float :false (niu:size-of :float 3) 0)
  (gl:enable-vertex-attrib-array 0)
  (loop
    until (glfw:window-should-close-p)
    do (progn
         ;; it is not necessary to create and bind and EBO
         (gl:draw-elements :triangles (niu:gl-array indices))
         (glfw:swap-buffers)
         (glfw:poll-events))))
