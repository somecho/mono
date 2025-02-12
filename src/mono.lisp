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
       (declare (ignorable fps))
       ,body)))

(defmacro with-loop (&body body)
  `(mono:with-frame-stats
     (loop until (glfw:window-should-close-p)
           do (progn ,@body)
           do (glfw:swap-buffers)
           do (glfw:poll-events))))

(defmacro with-return (binding &body body)
  "Similar to LET but takes only a single BINDING and returns it at the end of
the expression. Useful if a structure is declared first then mutated to be
returned later.

Example:
(mono:with-return (v (mono:empty-vec))
  (mono:push v 1)
  (mono:push v 2)); => #(1 2)
"
  (let ((sym (car `,binding)))
    `(let (,binding)
       ,@body
       ,sym)))
