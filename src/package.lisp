(defpackage #:mono
  (:use :cl :arrows)

  ;; mono
  (:export #:start
           #:with-fps)

  ;; array
  (:export #:size-of
           #:gl-array
           #:write-array-buffer)

  ;; shaders
  (:export #:+vs-projection+
           #:+fs-default+
           #:with-shader))
