(defpackage #:mono
  (:use :cl :arrows)
  (:shadow #:push)

  ;; mono
  (:export #:start
           #:with-frame-stats
           #:with-loop
           #:with-buffers
           #:with-gl-resources
           #:with-vao
           #:set-vertex-attrib)

  ;; array
  (:export #:size-of
           #:gl-array
           #:write-array-buffer
           #:flat-vec
           #:concat-vec
           #:empty-vec
           #:push
           #:concat-calls)

  ;; generators
  (:export #:gen-cuboid-vertices
           #:+cuboid-indices+)

  ;; math
  (:export #:pi-f
           #:clamp
           #:xyz-sphere*
           #:xyz-sphere
           #:sphere-xyz*
           #:sphere-xyz
           #:sample-tri*
           #:sample-tri)

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
