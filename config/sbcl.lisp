(in-package #:ctype)

(declaim (inline ratiop))
(defun ratiop (object) (sb-int:ratiop object))

(define-constant +floats+
    '((single-float . sb-int:single-float-p)
      (double-float . sb-int:double-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)) :test #'equal)
(define-constant +base-charset+ '((0 . 127)) :test #'equal)

(define-constant +string-uaets+ '(nil base-char character) :test #'equal)

(define-constant +complex-arrays-exist-p+ t)

(declaim (inline simple-array-p))
(defun simple-array-p (object) (sb-kernel:simple-array-p object))

(define-constant +class-aliases+ () :test #'equal)

(declaim (inline subclassp))
(defun subclassp (sub super) (member super (sb-mop:class-precedence-list sub)))

(declaim (inline typexpand))
(defun typexpand (type-specifier environment)
  (sb-ext:typexpand type-specifier environment))

(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     ((single-float) (sb-kernel:complex-single-float-p ,objectf))
     ((double-float) (sb-kernel:complex-double-float-p ,objectf))
     ((rational) (sb-kernel:complex-rational-p ,objectf))))
