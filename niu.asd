(asdf:defsystem #:niu
  :depends-on (:cl-opengl
               :cl-glfw3
               :arrows
               :cffi)
  :pathname "src"
  :serial t
  :components ((:file "package")))
