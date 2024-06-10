(in-package #:ctype)

(defmethod ctypep (client object (ct csatisfies))
  (declare (ignore client))
  (funcall (fdefinition (csatisfies-fname ct)) object))

(defmethod subctypep (client (ct1 csatisfies) (ct2 csatisfies))
  (declare (ignore client))
  (if (equal (csatisfies-fname ct1) (csatisfies-fname ct2))
      (values t t)
      (values nil nil)))
(defmethod ctype= (client (ct1 csatisfies) (ct2 csatisfies))
  (declare (ignore client))
  (if (equal (csatisfies-fname ct1) (csatisfies-fname ct2))
      (values t t)
      (values nil nil)))

(defmethod unparse ((ct csatisfies))
  `(satisfies ,(csatisfies-fname ct)))
