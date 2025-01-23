(defpackage #:niu/tests
  (:use :cl :parachute))

(in-package #:niu/tests)


(define-test buffer)

(define-test gl-array
  :parent buffer)

(define-test simple-vector
  :parent gl-array
  (is = 1 (gl::gl-array-size (niu:gl-array #(1.0))))
  (of-type gl:gl-array (niu:gl-array #(1.0)))
  (fail (= 0 (gl::gl-array-size (niu:gl-array #()))))
  (is eq :float (gl::gl-array-type (niu:gl-array #(1.0)))))

(define-test types-must-be-consistent
  :parent gl-array
  (fail (niu:gl-array #(1.0 1)))
  (fail (niu:gl-array #(#(1.0 2.0)
                        #(1.0 2)))))

(define-test dimensions-must-be-consistent
  :parent gl-array
  (fail (niu:gl-array #(#(1.0 2.0) #(1.0)))))

(define-test 2d-vector
  :parent gl-array
  (is = 4 (gl::gl-array-size
           (niu:gl-array #(#(1.0 2.0)
                           #(3.0 4.0)))))
  (is = 6 (gl::gl-array-size
           (niu:gl-array #(#(1.0 2.0 10.0)
                           #(3.0 4.0 -4.0)))))
  (is = 8 (gl::gl-array-size
           (niu:gl-array #(#(1.0 2.0 10.0 -0.1)
                           #(3.0 4.0 -4.0 0.1))))))

(define-test 2d-vector-p
  :parent buffer
  (false (niu::2d-vector-p #(1 0)))
  (true (niu::2d-vector-p #(#(1) #(2)))))

(test 'buffer)
