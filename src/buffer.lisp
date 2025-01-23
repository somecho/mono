(in-package #:niu)

(defun to-cffi-type (cl-type)
  "Returns the corresponding CFFI type. Supported CFFI types:
- :float
- :double"
  (ccase cl-type
    (single-float :float)
    (double-loat :double)))

(defun 2d-vector-p (v)
  "True if v is a simple-vector and the first element of v is also a simple
vector. Does not check if every element in v is a vector, or if their sizes are
equal."
  (and (simple-vector-p v)
       (simple-vector-p (aref v 0))))

(defun create-gl-array-2d (data)
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

(defun create-gl-array (data)
  "Allocates, writes and returns a cl-opengl:gl-array with data,
which is a simple-vector of some cffi-type. No check is done to make sure that
all types are the same."
  (let* ((data-type (type-of (aref data 0)))
         (gl-type (to-cffi-type data-type))
         (array-length (length data))
         (arr (gl:alloc-gl-array gl-type array-length)))
    (dotimes (i array-length)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defmacro gl-array (data)
  "Allocates, writes and returns a cl-opengl:gl-array with data. This macro
dispatches base on the type of data."
  (cond ((= (length data) 0) nil)
        ((2d-vector-p data) `(create-gl-array-2d ,data))
        ((simple-vector-p data) `(create-gl-array ,data))))

(defmacro write-array-buffer (id data)
  "Binds an array buffer to id and writes it with data and :dynamic-draw."
  `(progn
     (gl:bind-buffer :array-buffer id)
     (gl:buffer-data :array-buffer :dynamic-draw (niu:gl-array data))))
