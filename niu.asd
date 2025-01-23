(asdf:defsystem #:niu
  :depends-on (:cl-opengl
               :cl-glfw3
               :arrows
               :cffi)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "shaders")
               (:file "buffer")
               (:file "niu")))

(asdf:defsystem #:niu/tests
  :depends-on (:niu :parachute)
  :pathname "tests"
  :components ((:file "buffer.test")))
