(in-package #:ctype.ext.tfun)

(defgeneric array-element-ctype (array-type))
(defmethod array-element-ctype ((atype ctype)) (top))
(defmethod array-element-ctype ((atype conjunction))
  (apply #'conjoin (mapcar #'array-element-ctype (junction-ctypes atype))))
(defmethod array-element-ctype ((atype disjunction))
  (apply #'disjoin (mapcar #'array-element-ctype (junction-ctypes atype))))
(defmethod array-element-ctype ((atype negation))
  (negate (array-element-ctype (negation-ctype atype))))
(defmethod array-element-ctype ((atype carray)) (carray-eaet atype))

(define-tfun aref (array &rest indices)
  ;; TODO: return bot on index invalidity
  (single-value (array-element-ctype array)))
(define-tfun row-major-aref (array index)
  ;; TODO: return bot on index invalidity
  (single-value (array-element-ctype array)))
