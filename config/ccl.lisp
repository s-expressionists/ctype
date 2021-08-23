(in-package #:ctype)

(declaim (inline ratiop))
(defun ratiop (object) (ccl:ratiop object))

(define-constant +floats+
    ;; SHORT-FLOAT is a SINGLE-FLOAT on CCL, but the predicate is on short
    ;; float rather than single float for whatever reason.
    ;;   https://ccl.clozure.com/manual/chapter4.11.html#floating-point
    '((single-float . ccl::short-float-p)
      (double-float . ccl::double-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)) :test #'equal)
(define-constant +base-charset+ '((0 . 55295)) :test #'equal)

(define-constant +string-uaets+ '(nil base-char) :test #'equal)

(define-constant +complex-arrays-exist-p+ t)

(declaim (inline simple-array-p))
(defun simple-array-p (object) (ccl::simple-array-p object))

(define-constant +class-aliases+ () :test #'equal)

(declaim (inline subclassp))
(defun subclassp (sub super) (ccl::subclassp sub super))

(declaim (inline typexpand))
(defun typexpand (type-specifier environment)
  (ccl::type-expand type-specifier environment))

(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     ((single-float) (ccl::complex-single-float-p ,objectf))
     ((double-float) (ccl::complex-double-float-p ,objectf))))
