(asdf:defsystem #:mono
  :depends-on (:cl-opengl
               :cl-glfw3
               :arrows
               :cffi)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "shaders")
               (:file "array")
               (:file "generators")
               (:file "math")
               (:file "mono")
               (:module "glsl"
                :components ((:file "package")
                             (:file "simplex")))))

(asdf:defsystem #:mono/tests
  :depends-on (:mono :parachute)
  :pathname "tests"
  :components ((:file "buffer.test")))
