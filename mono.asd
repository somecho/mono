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
               (:file "mono")))

(asdf:defsystem #:mono/tests
  :depends-on (:mono :parachute)
  :pathname "tests"
  :components ((:file "buffer.test")))
