(in-package #:ctype.ext.tfun)

(defgeneric array-element-ctype (array-type))
(defdefaults array-element-ctype (array))
(defmethod array-element-ctype ((atype negation))
  (negate (array-element-ctype (negation-ctype atype))))
(defmethod array-element-ctype ((atype carray)) (carray-eaet atype))

(define-tfun aref (array &rest indices)
  ;; TODO: return bot on index invalidity
  (single-value (array-element-ctype array)))
(define-tfun row-major-aref (array index)
  ;; TODO: return bot on index invalidity
  (single-value (array-element-ctype array)))
