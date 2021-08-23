(in-package #:ctype)

(declaim (inline ratiop))
(defun ratiop (object) (ext:ratiop object))

(define-constant +floats+
    '((single-float . kernel:single-float-p)
      (double-float . kernel:double-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)))
(define-constant +base-charset+ '((0 . 65535)))

(define-constant +string-uaets+ '(base-char) :test #'equal)

(define-constant +complex-arrays-exist-p+ t)

(declaim (inline simple-array-p))
(defun simple-array-p (object) (kernel:simple-array-p object))

(define-constant +class-aliases+ () :test #'equal)

(declaim (inline subclassp))
(defun subclassp (sub super)
  (member super (kernel:std-compute-class-precedence-list sub)))

(declaim (inline typexpand))
(defun typexpand (type-specifier environment)
  (declare (ignore environment))
  (kernel:type-expand type-specifier))

(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     ((single-float) (kernel:complex-single-float-p ,objectf))
     ((double-float) (kernel:complex-double-float-p ,objectf))
     ((rational) (kernel:complex-rational-p ,objectf))))
