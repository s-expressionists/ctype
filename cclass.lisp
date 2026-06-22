(in-package #:ctype)

(defmethod ctypep (client object (ct cclass))
  (subclassp client (class-of object) (cclass-class ct)))

(defmethod subctypep (client (ct1 cclass) (ct2 cclass))
  ;; NOTE: If ctypes are supposed to work in the face of future redefinitions,
  ;; this should return NIL NIL except with unredefinable classes.
  (values (subclassp client (cclass-class ct1) (cclass-class ct2)) t))

(defmethod ctype= (client (ct1 cclass) (ct2 cclass))
  (declare (ignore client))
  (values (eql (cclass-class ct1) (cclass-class ct2)) t))

(defmethod cofinitep (client (ct cclass))
  (declare (ignore client))
  (values nil t))

;;; These classes are defined as disjoint in CLHS 4.2.2.
;;; cons, array, number, and character are not handled as cclasses
;;; so they don't appear here. function sometimes sort of is.
;;; Condition may not be a class (although it usually is).
;;; FIXME? Make client-specializable?
(defvar *disjoint-classes*
  '(symbol hash-table function readtable package pathname
    stream random-state restart condition
    ;; These appear AFTER the system classes, so that even if one of the
    ;; system classes is a subclass of structure-object or whatever, it can
    ;; be understood to be disjoint from user classes.
    structure-object standard-class))

(defmethod disjointp (client (ct1 cclass) (ct2 cclass))
  ;; Pick off cases defined by 4.2.2.
  (flet ((scp (c1 cn2) (subclassp client c1 (find-class client cn2))))
    (let ((supct1 (find (cclass-class ct1) *disjoint-classes* :test #'scp))
          (supct2 (find (cclass-class ct2) *disjoint-classes* :test #'scp)))
      (if (and supct1 supct2 (not (eq supct1 supct2)))
          (values t t)
          (values nil nil)))))

(defmethod conjoin/2 (client (ct1 cclass) (ct2 cclass))
  (let ((c1 (cclass-class ct1)) (c2 (cclass-class ct2)))
    (cond ((eq c1 c2) ct1)
          ((disjointp client ct1 ct2) (bot))
          ;; These classes may have a common subclass. Who knows?
          ;; (Strictly speaking we could check...)
          (t nil))))

(defmethod disjoin/2 (client (ct1 cclass) (ct2 cclass))
  (declare (ignore client))
  (let ((c1 (cclass-class ct1)) (c2 (cclass-class ct2)))
    (cond ((eq c1 c2) ct1)
          (t nil))))

(defmethod unparse ((ct cclass))
  (class-name (cclass-class ct)))
