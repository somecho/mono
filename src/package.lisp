(defpackage #:niu
  (:use :cl :arrows)
  (:export
   #:size-of
   #:gl-array
   #:gl-array-2d
   #:gl-array-length
   #:with-shader
   #:run-sketch
   #:write-array-buffer
   #:write-array-buffer-2d))


(in-package #:niu)



(defconstant +cl-cffi-type-map+
  `(single-float :float
    double-float :double)
  "Common lisp and CFFI type pairs")


(defun to-cffi-type (cl-type)
  "Returns the corresponding CFFI type."
  (getf +cl-cffi-type-map+ cl-type))


(defmacro with-shader (symbol vertex-shader-source fragment-shader-source &body body)
  "Creates a form that compiles a vertex and fragment shader from source, links
it and makes it available throughout the form."
  `(handler-case
       (let ((vs (gl:create-shader :vertex-shader))
             (fs (gl:create-shader :fragment-shader))
             (,symbol (gl:create-program)))
         (gl:shader-source vs ,vertex-shader-source)
         (gl:shader-source fs ,fragment-shader-source)
         (format t "Compiling vertex shader...~%")
         (gl:compile-shader vs)
         (format t "Compiling fragment shader...~%")
         (gl:compile-shader fs)
         (format t "Attaching vertex shader...~%")
         (gl:attach-shader ,symbol vs)
         (format t "Attaching fragment shader...~%")
         (gl:attach-shader ,symbol fs)
         (format t "Linking shader program~%")
         (gl:link-program ,symbol)
         (gl:delete-shader vs)
         (gl:delete-shader fs)
         ,@body
         (gl:delete-program ,symbol))
     (error (c) (print c))))


(defmacro run-sketch ((&rest keys) &body body)
  "Creates and starts GLFW window context.
Keys that can be provided and their defaults:
  - :title - \"\"
  - :width - 1000
  - :height - 1000
  - :vertex-shader - niu:+vs-default+
  - :fragment-shader - niu:+fs-white+
  - :on-keydown - (lambda (key))"
  (let ((title (getf keys :title ""))
        (width (getf keys :width 1000))
        (height (getf keys :height 1000))
        (vertex-shader (getf keys :vertex-shader +vs-default+))
        (fragment-shader (getf keys :fragment-shader +fs-white+))
        (on-keydown (getf keys :on-keydown (lambda (key)))))
    `(progn
       (glfw:def-key-callback key-callback (win key scancode action mod)
         (declare (ignore scancode mod))
         (when (eq action :press)
           (progn
             (handler-case (funcall ',on-keydown key) (error (e) (print e)))
             (when (eq key :escape)
               (glfw:set-window-should-close win)))))
       (glfw:with-init-window (:title ,title :width ,width :height ,height)
         (with-shader default-shader ,vertex-shader ,fragment-shader
           (glfw:set-key-callback 'key-callback)
           (gl:viewport 0 0 ,width ,height)
           ,@body)))))


(defun size-of (foreign-type &optional (n 1))
  "Calculates the size of a C type. An optional argument
`n` can be provided as the number of said type."
  (* n (cffi:foreign-type-size foreign-type)))

(defun gl-array-length (array)
  (let* ((e (gl:glaref array 0))
         (element-size (-> (type-of e) (to-cffi-type) (sizeof)))
         (array-size (gl:gl-array-byte-size array)))
    (/ array-size element-size)))

(defun write-array-buffer (id data)
  (gl:bind-buffer :array-buffer id)
  (gl:buffer-data :array-buffer :dynamic-draw (niu:gl-array data)))


(defun gl-array (data)
  (let* ((data-type (type-of (aref data 0)))
         (gl-type (to-cffi-type data-type))
         (array-length (length data))
         (arr (gl:alloc-gl-array gl-type array-length)))
    (dotimes (i array-length)
      (setf (gl:glaref arr i) (aref data i)))
    arr))
