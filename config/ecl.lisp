(in-package #:ctype)

(declaim (inline ratiop))
(defun ratiop (object) (si:ratiop object))

(define-constant +floats+
    '((single-float . si:single-float-p)
      (double-float . si:double-float-p)
      (long-float . si:long-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)) :test #'equal)
(define-constant +base-charset+ '((0 . 255)) :test #'equal)

(define-constant +string-uaets+ '(base-char character) :test #'equal)

(define-constant +complex-arrays-exist-p+ t)

(declaim (inline simple-array-p))
(defun simple-array-p (object)
  (si::simple-array-p object))

(define-constant +class-aliases+ () :test #'equal)

(declaim (inline subclassp))
(defun subclassp (sub super) (si::subclassp sub super))

;;; This is like si::normalize-type, except we return a type specifier and
;;; whether it expanded, and don't signal an error if something is malformed.
;;; This obviously uses internals - fragile - but ECL doesn't export this.
(defun typexpand-1 (spec env)
  (declare (ignore env))
  (cond ((symbolp spec)
         (let ((expander (si:get-sysprop spec 'si::deftype-definition)))
           (if expander
               (values (funcall expander nil) t)
               (values spec nil))))
        ((consp spec)
         (let* ((head (car spec)) (args (cdr spec))
                (expander (si:get-sysprop head 'si::deftype-definition)))
           (if expander
               (values (funcall expander args) t)
               (values spec nil))))
        (t (values spec nil))))

(defun typexpand (type-specifier environment)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expandedp)
               (typexpand-1 type-specifier environment)
             (if expandedp
                 (setf ever-expanded t type-specifier expansion)
                 (return (values type-specifier ever-expanded))))))

(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     ((single-float) (typep ,objectf 'si:complex-single-float))
     ((double-float) (typep ,objectf 'si:complex-double-float))
     ((long-float) (typep ,objectf 'si:complex-long-float))))
