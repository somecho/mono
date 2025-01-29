(in-package #:mono)

(defun gen-cuboid-vertices (width &optional (height 1) (depth 1))
  (let ((cw (* width 0.5))
        (ch (* height 0.5))
        (cd (* depth 0.5))
        (-cw (* width -0.5))
        (-ch (* height -0.5))
        (-cd (* depth -0.5)))
    (vector
     -cw -ch -cd
     cw -ch -cd
     cw  ch -cd
     -cw  ch -cd
     -cw -ch cd
     cw -ch cd
     cw ch cd
     -cw ch cd)))

(defconstant +cuboid-indices+ #(0 1 2 0 2 3
                                4 5 6 4 6 7
                                0 4 7 0 7 3
                                1 5 6 1 6 2
                                3 2 6 3 6 7
                                0 1 5 0 5 4))
