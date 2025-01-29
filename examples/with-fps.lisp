;;; with-fps.lisp

;;; This is an example of how to use the mono:with-fps macro.

(ql:quickload :mono)
(mono:start ()
  (mono:with-frame-stats
    (loop until (glfw:window-should-close-p)
          do (format t "FPS: ~a FRAME NUMBER: ~a TIME: ~a DELTA TIME: ~a~%"
                     mono::fps mono::frame-num mono::curr-time mono::frame-delta)
          do (glfw:swap-buffers)
          do (glfw:poll-events))))
