(ql:quickload :glkit)
(defpackage :uniform (:use :cl))
(in-package :uniform)

;; Showcases using orthographic projection

(defparameter width 800)
(defparameter height 600)

;; using screen space coordinates and not normalized
(defparameter vertices  (vector
                         (* width 0.25)  (* height 0.25) 0.0
                         (* width 0.25) (* height 0.75) 0.0
                         (* width 0.75) (* height 0.75) 0.0
                         (* width 0.75) (* height 0.25) 0.0))

(defparameter indices #(0 1 3 1 2 3))

(defparameter vbo nil)
(defparameter vao nil)

(defparameter ortho (kit.glm:ortho-matrix
                     0 width 0 height -1 1))

(mono:start (:vertex-shader mono:+vs-projection+
             :width width
             :height height)
  (setf vbo (gl:gen-buffer))
  (setf vao (gl:gen-vertex-array))
  (gl:bind-vertex-array vao)
  (mono:write-array-buffer vbo (mono:gl-array vertices))
  (gl:vertex-attrib-pointer 0 3 :float :false (mono:size-of :float 3) 0)
  (gl:enable-vertex-attrib-array 0)
  (let ((mat (gl:get-uniform-location
              mono::default-shader "projectionMatrix")))
    (gl:uniform-matrix-4fv
     ;; row to column major
     mat (kit.glm:transpose-matrix ortho)))
  (loop
    until (glfw:window-should-close-p)
    do (progn
         (gl:draw-elements
          :triangles (mono:gl-array indices :unsigned-int))
         (glfw:swap-buffers)
         (glfw:poll-events))))
