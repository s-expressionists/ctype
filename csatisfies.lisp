(in-package #:ctype)

(defmethod ctypep (object (ct csatisfies))
  (funcall (fdefinition (csatisfies-fname ct)) object))

(defmethod subctypep ((ct1 csatisfies) (ct2 csatisfies))
  (if (equal (csatisfies-fname ct1) (csatisfies-fname ct2))
      (values t t)
      (call-next-method)))

(defmethod unparse ((ct csatisfies))
  `(satisfies ,(csatisfies-fname ct)))
