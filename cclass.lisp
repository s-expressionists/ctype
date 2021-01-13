(in-package #:ctype)

(defmethod ctypep (object (ct cclass))
  (core:subclassp (class-of object) (cclass-class ct)))

(defmethod subctypep ((ct1 cclass) (ct2 cclass))
  ;; NOTE: If ctypes are supposed to work in the face of future redefinitions,
  ;; this should return NIL NIL except with unredefinable classes.
  (values (core:subclassp (cclass-class ct1) (cclass-class ct2)) t))

(defmethod conjoin/2 ((ct1 cclass) (ct2 cclass))
  (let ((c1 (cclass-class ct1)) (c2 (cclass-class ct2)))
    (cond ((eq c1 c2) ct1)
          ;; These classes may have a common subclass. Who knows?
          ;; (Strictly speaking we could check...)
          (t nil))))

(defmethod disjoin/2 ((ct1 cclass) (ct2 cclass))
  (let ((c1 (cclass-class ct1)) (c2 (cclass-class ct2)))
    (cond ((eq c1 c2) ct1)
          (t nil))))

(defmethod unparse ((ct cclass))
  (class-name (cclass-class ct)))
