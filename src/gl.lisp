(in-package #:mono)

(defun uniformf (name value)
  "Sets a uniform float to the current bound shader program."
  (declare (type (simple-array character) name)
           (type single-float value))
  (-> (gl:get-integer :current-program)
      (gl:get-uniform-location name)
      (gl:uniformf value)))

(defun uniform-mat4f (name mat)
  "Sets a uniform 4x4 float matrix to the current bound shader program."
  (declare (type (simple-array character) name)
           (type (simple-array single-float (16)) mat))
  (-> (gl:get-integer :current-program)
      (gl:get-uniform-location name)
      (gl:uniform-matrix-4fv mat)))

(defmacro with-buffers (buffers &body body)
  "Generates buffers with (gl:gen-buffer) for each symbol in BUFFERS. Must be
called within a GL context."
  `(progn
     ,@(mapcar (lambda (buf)
                 `(defparameter ,buf (gl:gen-buffer)))
               buffers)
     ,@body))

(defmacro with-gl-resources ((&rest keys) &body body)
  "Generates OpenGL resources that are available for use in the expression.

Example:
(mono:with-gl-resources (:buffers (vbo1 vbo2)
                         :vertex-arrays (vao1 vao2))
  (mono:with-vao vao1
    (mono:write-array-buffer vbo1 DATA)))
"
  (let ((buffers (getf keys :buffers '()))
        (vertex-arrays (getf keys :vertex-arrays '())))
    `(progn
       ,@(mapcar (lambda (buf) `(defparameter ,buf (gl:gen-buffer))) buffers)
       ,@(mapcar (lambda (vao) `(defparameter ,vao (gl:gen-vertex-array))) vertex-arrays)
       ,@body
       (gl:delete-buffers (list ,@buffers))
       (gl:delete-vertex-arrays (list ,@vertex-arrays)))))

(defmacro with-vao (vao &body body)
  "Convenience macro that binds a VAO for the duration of the expression and
unbinds it at the end.

Example:

(mono:with-gl-resources (:vertex-arrays (vao))
  (mono:with-vao vao (gl:draw-arrays :triangles 0 3)))
"
  `(progn
     (gl:bind-vertex-array ,vao)
     ,@body
     (gl:bind-vertex-array 0)))

(defun set-vertex-attrib (size &rest keys)
  "Convenience function for calling gl:vertex-attrib-pointer and
gl:enable-vertex-attrib-array. Use when only size and type information is needed.

Example:

;; Sets vertex attrib pointer with index 1 to be of size 3 and type :float
(mono:set-vertex-attrib 3 :type :float :id 1)
"
  (let ((index (getf keys :index 0))
        (type (getf keys :type :float)))
    (gl:vertex-attrib-pointer index size type :false (mono:size-of type size) 0)
    (gl:enable-vertex-attrib-array index)))
