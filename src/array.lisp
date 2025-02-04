(in-package #:mono)

(defun --to-cffi-type (cl-type)
  "Returns the corresponding CFFI type. Supported CFFI types:
- :float
- :double"
  (declare
   (type (member single-float double-float bit integer) cl-type))
  (ccase cl-type
    (bit :unsigned-int)
    (integer :unsigned-int)
    (single-float :float)
    (double-float :double)))

(defun size-of (foreign-type &optional (n 1))
  "Calculates the size of a C type. An optional argument
`n` can be provided as the number of said type."
  (* n (cffi:foreign-type-size foreign-type)))

(declaim (ftype (function
                 ((or (simple-vector) (vector)) &optional (member :float :double :unsigned-int)) gl:gl-array)
                gl-array))
(defun gl-array (data &optional foreign-type)
  "Allocates, writes and returns a cl-opengl:gl-array with data,
which is a simple-vector of some cffi-type. No check is done to make sure that
all types are the same. Optionally provide a TYPE."
  (let* ((data-type (type-of (aref data 0)))
         (gl-type (if foreign-type
                      foreign-type
                      (--to-cffi-type data-type)))
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
