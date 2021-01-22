(in-package #:ctype)

(defmethod ctypep (object (ct cclass))
  (subclassp (class-of object) (cclass-class ct)))

(defmethod subctypep ((ct1 cclass) (ct2 cclass))
  ;; NOTE: If ctypes are supposed to work in the face of future redefinitions,
  ;; this should return NIL NIL except with unredefinable classes.
  (values (subclassp (cclass-class ct1) (cclass-class ct2)) t))

(defmethod cofinitep ((ct cclass)) (values nil t))

;;; These classes are defined as disjoint in CLHS 4.2.2.
;;; cons, array, number, and character are not handled as cclasses
;;; so they don't appear here. function sometimes sort of is.
;;; condition may not be a class.
;;; FIXME: Refers to environment
(defparameter *disjoint-classes*
  (list (find-class 'symbol) (find-class 'hash-table) (find-class 'function)
        (find-class 'readtable) (find-class 'package) (find-class 'pathname)
        (find-class 'stream) (find-class 'random-state) (find-class 'restart)))

(defmethod disjointp ((ct1 cclass) (ct2 cclass))
  ;; Pick off cases defined by 4.2.2.
  (let ((class1 (cclass-class ct1)) (class2 (cclass-class ct2)))
    (let ((supct1 (find class1 *disjoint-classes* :test #'subclassp))
          (supct2 (find class2 *disjoint-classes* :test #'subclassp)))
      (if (and supct1 supct2 (not (eq supct1 supct2)))
          (values t t)
          (call-next-method)))))

(defmethod conjoin/2 ((ct1 cclass) (ct2 cclass))
  (let ((c1 (cclass-class ct1)) (c2 (cclass-class ct2)))
    (cond ((eq c1 c2) ct1)
          ((disjointp ct1 ct2) (bot))
          ;; These classes may have a common subclass. Who knows?
          ;; (Strictly speaking we could check...)
          (t nil))))

(defmethod disjoin/2 ((ct1 cclass) (ct2 cclass))
  (let ((c1 (cclass-class ct1)) (c2 (cclass-class ct2)))
    (cond ((eq c1 c2) ct1)
          (t nil))))

(defmethod unparse ((ct cclass))
  (class-name (cclass-class ct)))
