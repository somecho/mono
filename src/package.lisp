(defpackage #:mono
  (:use :cl :arrows)
  (:shadow #:push)

  ;; mono
  (:export #:start
           #:with-frame-stats
           #:with-loop
           #:with-return)

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
           #:sample-tri
           #:sample-tri-strip
           #:sample-tri-fan)

  ;; camera
  (:export #:camera
           #:update)

  ;; gl
  (:export #:uniformf
           #:uniform2f
           #:uniform-mat4f
           #:with-buffers
           #:with-gl-resources
           #:with-vao
           #:set-vertex-attrib)

  ;; shaders
  (:export #:+vs-projection+
           #:+vs-default+
           #:+fs-default+
           #:+fs-white+
           #:with-shader
           #:compile-shader
           #:create-shader))
