(in-package #:ctype)

(define-method-combination basic (operator)
  ((around (:around))
   (primary () :required t))
  "This is like standard short form combinations, except
* You write basic operator instead of just operator to refer to the combo.
* You don't need to put the operator name as a qualifier.
* :order and :identity-with-one-argument are not supported because I don't need them.
This is based on the BASIC combination in sellout's method-combination-utilities library."
  (let ((form `(,operator
                ,@(loop for prim in primary collect `(call-method, prim)))))
    (if around
        `(call-method ,(first around) (,@(rest around) (make-method ,form)))
        form)))
