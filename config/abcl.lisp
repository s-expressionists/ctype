(in-package #:ctype)

(defun ratiop (object) (typep object 'ratio))

(defun single-float-p (number)
  (and (floatp number)
       (<= most-negative-single-float number most-positive-single-float)))

(defun double-float-p (number)
  (and (floatp number)
       (< number most-negative-single-float)
       (> number most-positive-single-float)
       (>= number most-negative-double-float)
       (<= number most-positive-single-float)))

(define-constant +floats+
    '((single-float . single-float-p)
      (double-float . double-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)) :test #'equal)
(define-constant +base-charset+ '((0 . 127)) :test #'equal)
(define-constant +string-uaets+ '(nil base-char character) :test #'equal)

(define-constant +complex-arrays-exist-p+ t)

(defun simple-array-p (object) (typep object 'simple-array))

(define-constant +class-aliases+ () :test #'equal)

(defun subclassp (sub super) (member super (mop:class-precedence-list sub)))

(defun typexpand (type-specifier environment)
  (declare (ignore environment))
  (system:normalize-type type-specifier))

(defmacro complex-ucptp (objectf ucpt)
  (declare (ignore objectf))
  `(ecase ,ucpt
     ((*) t)))
