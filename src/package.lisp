(defpackage #:mono
  (:use :cl :arrows)

  ;; mono
  (:export #:start
           #:with-frame-stats
           #:with-loop)

  ;; array
  (:export #:size-of
           #:gl-array
           #:write-array-buffer)

  ;; generators
  (:export #:gen-cuboid-vertices
           #:+cuboid-indices+)

  ;; math
  (:export #:pi-f
           #:clamp
           #:xyz-sphere*
           #:xyz-sphere
           #:sphere-xyz*
           #:sphere-xyz)

  ;; camera
  (:export #:camera
           #:update)

  ;; gl
  (:export #:uniformf
           #:uniform-mat4f)

  ;; shaders
  (:export #:+vs-projection+
           #:+vs-default+
           #:+fs-default+
           #:+fs-white+
           #:with-shader))
