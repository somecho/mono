(ql:quickload :mono)

(defparameter cube-vertices (mono:gl-array (mono:gen-cuboid-vertices 1.0)))
(defparameter cube-indices (mono:gl-array mono:+cuboid-indices+))
(defparameter vbo nil)
(defparameter w 1000)
(defparameter h 1000)
(defparameter proj (kit.glm:perspective-matrix 45 (/ w h) 0.1 100))
(defparameter cam (make-instance 'mono:camera :pos (kit.glm:vec3 3.0)))

(mono:start (:width w :height h :vertex-shader mono:+vs-projection+)
  (setf vbo (gl:gen-buffer))
  (mono:write-array-buffer vbo cube-vertices)
  (gl:vertex-attrib-pointer 0 3 :float :false (mono:size-of :float 3) 0)
  (gl:enable-vertex-attrib-array 0)
  (gl:polygon-mode :front-and-back :line)
  (mono:with-frame-stats
    (loop
      until (glfw:window-should-close-p)
      do (progn
           (gl:clear-color 0.0 0.0 0.0 1.0)
           (gl:clear :color-buffer-bit)
           (mono:update cam)
           (let* ((umat (gl:get-uniform-location mono::default-shader "projectionMatrix"))
                  (mat (kit.glm:matrix* proj (mono::view-mat cam))))
             (gl:uniform-matrix-4fv umat (kit.glm:transpose-matrix mat)))
           (gl:draw-elements :triangles cube-indices)
           (glfw:swap-buffers)
           (glfw:poll-events)))))
