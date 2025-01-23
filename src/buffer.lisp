(in-package #:niu)

(defun to-cffi-type (cl-type)
  "Returns the corresponding CFFI type. Supported CFFI types:
- :float
- :double"
  (declare
   (type (member single-float double-float) cl-type))
  (ccase cl-type
    (single-float :float)
    (double-float :double)))

(defun gl-array-2d (data)
  "Allocates, writes and returns a cl-opengl:gl-array with data,
which is a simple-vector of simple-vectors. No check is done to make sure that
every vector in data is of the same size. The stride of the allocated gl array
will be the length of the first vector in data."
  (let* ((dimension (length (aref data 0)))
         (data-type (type-of (aref (aref data 0) 0)))
         (gl-type (to-cffi-type data-type))
         (array-length (* (length data) dimension))
         (arr (gl:alloc-gl-array gl-type array-length)))
    (dotimes (i (length data))
      (dotimes (j dimension)
        (setf (gl:glaref arr (+ (* i dimension) j))
              (aref (aref data i) j))))
    arr))

(defun gl-array (data)
  "Allocates, writes and returns a cl-opengl:gl-array with data,
which is a simple-vector of some cffi-type. No check is done to make sure that
all types are the same."
  (declare (type simple-vector data))
  (let* ((data-type (type-of (aref data 0)))
         (gl-type (to-cffi-type data-type))
         (array-length (length data))
         (arr (gl:alloc-gl-array gl-type array-length)))
    (dotimes (i array-length)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defun write-array-buffer (id data)
  "Binds an array buffer to id and writes it with data and :dynamic-draw. DATA
must be a cl-opengl:gl-array."
  (declare (type (unsigned-byte 32) id) (type gl:gl-array data))
  (gl:bind-buffer :array-buffer id)
  (gl:buffer-data :array-buffer :dynamic-draw data))
