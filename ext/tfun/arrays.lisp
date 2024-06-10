(in-package #:ctype.ext.tfun)

(defgeneric array-element-ctype (client array-type))
(defdefaults array-element-ctype (c array))
(defmethod array-element-ctype (c (atype negation))
  (negate c (array-element-ctype (negation-ctype atype))))
(defmethod array-element-ctype (c (atype carray))
  (declare (ignore c))
  (carray-eaet atype))

(define-tfun aref (client array &rest indices)
  ;; TODO: return bot on index invalidity
  (single-value (array-element-ctype client array)))
(define-tfun row-major-aref (client array index)
  ;; TODO: return bot on index invalidity
  (single-value (array-element-ctype client array)))
