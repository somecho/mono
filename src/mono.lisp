(in-package #:mono)

(defmacro start ((&rest keys) &body body)
  "Creates and starts GLFW window context.
Keys that can be provided and their defaults:
  - :title - \"\"
  - :width - 1000
  - :height - 1000
  - :vertex-shader - mono:+vs-default+
  - :fragment-shader - mono:+fs-white+
  - :on-keydown - (lambda (key))"
  (let ((title (getf keys :title ""))
        (width (getf keys :width 1000))
        (height (getf keys :height 1000))
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
       (glfw:with-init-window (:title ,title :width ,width :height ,height)
         (with-shader default-shader ,vertex-shader ,fragment-shader
           (glfw:set-key-callback 'key-callback)
           (gl:viewport 0 0 ,width ,height)
           ,@body)))))

(defmacro with-fps ((symbol) &body loop-expr)
  "Instruments a loop expression to calculate the framerate. LOOP-EXPR must be a
loop expression! WITH-FPS must be called after GLFW has been initialized."
  (let* ((loop-body (car `,loop-expr))
         (update-form `(do (let ((new-time (glfw:get-time)))
                             (setf frame-delta (- new-time curr-time))
                             (setf curr-time new-time)
                             (setf ,symbol (floor (/ 1.0 frame-delta))))))
         (body (concatenate 'list loop-body update-form)))
    `(let ((frame-delta 0.0)
           (curr-time (glfw:get-time))
           (,symbol 0))
       ,body)))
