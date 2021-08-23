(in-package #:ctype)

(defconstant +distinct-short-float-zeroes-p+ (not (eql -0s0 0s0)))
(defconstant +distinct-single-float-zeroes-p+ (not (eql -0f0 0f0)))
(defconstant +distinct-double-float-zeroes-p+ (not (eql -0d0 0d0)))
(defconstant +distinct-long-float-zeroes-p+ (not (eql -0L0 0L0)))

(defmacro range-kindp (objectf kindf)
  `(ecase ,kindf
     ((integer) (integerp ,objectf))
     ((ratio) (ratiop ,objectf))
     ,@(loop for (kind . pred) in +floats+
             collect `((,kind) (,pred ,objectf)))))
