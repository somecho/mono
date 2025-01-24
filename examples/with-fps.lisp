;;; with-fps.lisp

;;; This is an example of how to use the mono:with-fps macro.

(mono:start ()
  (mono:with-fps (fps)
    (loop until (glfw:window-should-close-p)
          do (print fps)
          do (glfw:swap-buffers)
          do (glfw:poll-events))))
