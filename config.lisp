(in-package #:ctype)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reconstant (name value test)
    (if (not (boundp name))
        value
        (let ((old (symbol-value name)))
          (if (funcall test value old)
              old
              (error "Cannot redefine constant ~a to ~a" name value))))))

(defmacro define-constant (name value &key (test ''eql))
  `(defconstant ,name (reconstant ',name ,value ,test)))

;;;

(defconstant +ratiop+
  #+clasp 'ext::ratiop
  #-(or clasp) (error "RATIOP not defined for implementation"))

(define-constant +floats+
  #+clasp '((single-float . core:single-float-p)
            (double-float . core:double-float-p))
  #-(or clasp) (error "FLOATS not defined for implementation")
  :test #'equal)

(defun subclassp (sub super)
  #+clasp (core:subclassp sub super)
  #+(or) (member super (mop:class-precedence-list sub))
  #-(or clasp) (error "SUBCLASSP not defined for implementation"))

(defmacro complex-ucptp (objectf ucpt)
  (declare (ignorable objectf))
  `(ecase ,ucpt
     ((*) t)
     #+clasp ,@()
     #-(or clasp) ,(error "COMPLEX-UCPTP not defined for implementation")))

;;;

(defmacro range-kindp (objectf kindf)
  `(ecase ,kindf
     ((integer) (integerp ,objectf))
     ((ratio) (,+ratiop+ ,objectf))
     ,@(loop for (kind . pred) in +floats+
             collect `((,kind) (,pred ,objectf)))))
