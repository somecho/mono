(in-package #:mono)

(defmacro start ((&rest keys) &body body)
  "Creates and starts GLFW window context.
Keys that can be provided and their defaults:
  - :title - \"\"
  - :width - 1000
  - :height - 1000
  - :samples - 0
  - :vertex-shader - mono:+vs-default+
  - :fragment-shader - mono:+fs-white+
  - :on-keydown - (lambda (key))"
  (let ((title (getf keys :title ""))
        (width (getf keys :width 1000))
        (height (getf keys :height 1000))
        (samples (getf keys :samples 0))
        (vertex-shader (getf keys :vertex-shader +vs-default+))
        (fragment-shader (getf keys :fragment-shader +fs-white+))
        (on-keydown (getf keys :on-keydown (lambda (key)))))
    `(progn
       (glfw:def-key-callback key-callback (win key scancode action mod)
         (declare (ignore scancode mod))
         (when (eq action :press)
           (progn
             (handler-case (funcall ',on-keydown key) (error (e) (print e)))
             (when (eq key :escape)
               (glfw:set-window-should-close win)))))
       (glfw:with-init-window (:title ,title :width ,width :height ,height :samples ,samples)
         (with-shader default-shader ,vertex-shader ,fragment-shader
           (glfw:set-key-callback 'key-callback)
           (gl:viewport 0 0 ,width ,height)
           ,@body)))))

(defmacro with-frame-stats (&body loop-expr)
  "Instruments a loop expression to calculate the framerate. LOOP-EXPR must be a
loop expression! WITH-FRAME-STATS must be called after GLFW has been
initialized. This macro makes the following symbols available:
- mono::fps - rounded frame rate
- mono::frame-num - the current frame number
- mono::frame-delta - the duration in seconds it took to render the previous
  frame
- mono::curr-time - the current time in seconds since GLFW has been initalized"
  (let* ((loop-body (car `,loop-expr))
         (update-form `(do (let ((new-time (glfw:get-time)))
                             (setf frame-delta (- new-time curr-time))
                             (setf curr-time new-time)
                             (setf fps (round (/ 1.0 frame-delta)))
                             (incf frame-num))))
         (body (concatenate 'list loop-body update-form)))
    `(let ((frame-delta 0.0)
           (curr-time (glfw:get-time))
           (fps 0)
           (frame-num 0))
       ,body)))

(defmacro with-loop (&body body)
  `(mono:with-frame-stats
     (loop until (glfw:window-should-close-p)
           do (progn ,@body)
           do (glfw:swap-buffers)
           do (glfw:poll-events))))

(defmacro with-buffers (buffers &body body)
  "Generates buffers with (gl:gen-buffer) for each symbol in BUFFERS. Must be
called within a GL context."
  `(progn
     ,@(mapcar (lambda (buf)
                 `(defparameter ,buf (gl:gen-buffer)))
               buffers)
     ,@body))

(defmacro with-gl-resources ((&rest keys) &body body)
  "Generates OpenGL resources that are available for use in the expression.

Example:
(mono:with-gl-resources (:buffers (vbo1 vbo2)
                         :vertex-arrays (vao1 vao2))
  (mono:with-vao vao1
    (mono:write-array-buffer vbo1 DATA)))
"
  (let ((buffers (getf keys :buffers '()))
        (vertex-arrays (getf keys :vertex-arrays '())))
    `(progn
       ,@(mapcar (lambda (buf) `(defparameter ,buf (gl:gen-buffer))) buffers)
       ,@(mapcar (lambda (vao) `(defparameter ,vao (gl:gen-vertex-array))) vertex-arrays)
       ,@body
       (gl:delete-buffers (list ,@buffers))
       (gl:delete-vertex-arrays (list ,@vertex-arrays)))))

(defmacro with-vao (vao &body body)
  "Convenience macro that binds a VAO for the duration of the expression and
unbinds it at the end.

Example:

(mono:with-gl-resources (:vertex-arrays (vao))
  (mono:with-vao vao (gl:draw-arrays :triangles 0 3)))
"
  `(progn
     (gl:bind-vertex-array ,vao)
     ,@body
     (gl:bind-vertex-array 0)))

(defun set-vertex-attrib (size &rest keys)
  "Convenience function for calling gl:vertex-attrib-pointer and
gl:enable-vertex-attrib-array. Use when only size and type information is needed.

Example:

;; Sets vertex attrib pointer with index 1 to be of size 3 and type :float
(mono:set-vertex-attrib 3 :type :float :id 1)
"
  (let ((index (getf keys :index 0))
        (type (getf keys :type :float)))
    (gl:vertex-attrib-pointer index size type :false (mono:size-of type size) 0)
    (gl:enable-vertex-attrib-array index)))
