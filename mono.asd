(asdf:defsystem #:mono
  :depends-on (:cl-opengl
               :cl-glfw3
               :glkit
               :arrows
               :cffi)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "shaders")
               (:file "array")
               (:file "generators")
               (:file "math")
               (:file "camera")
               (:file "gl")
               (:file "mono")
               (:module "glsl"
                :components ((:file "package")
                             (:file "simplex")
                             (:file "color")))))

(asdf:defsystem #:mono/tests
  :depends-on (:mono :parachute)
  :pathname "tests"
  :components ((:file "buffer.test")))
