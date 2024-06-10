(in-package #:ctype.ext.data-structures)

(defclass clist-of (ctype)
  ((%etype :initarg :etype :reader element-ctype))
  (:documentation "Proper homogeneous list ctype."))

(defmethod unparse ((object clist-of))
  `(list-of ,(unparse (element-ctype object))))

(defun clist-of (ect)
  (if (bot-p ect)
      (cmember nil)
      (make-instance 'clist-of :etype ect)))

(define-extended-type list-of (element-type &environment env)
  :documentation "A proper list whose elements are all of type ELEMENT-TYPE."
  :simple ((declare (ignore element-type env))
           'list)
  :extended
  ((clist-of (extended-specifier-ctype element-type env))))

(defmethod ctypep (client (object null) (ct clist-of))
  (declare (ignore client))
  t)
(defmethod ctypep (client (object cons) (ct clist-of))
  (let ((ect (element-ctype ct)))
    (and (ctypep client (car object) ect)
         ;; Traverse circular lists carefully
         (loop for sub = (cdr object) then (cdr sub)
               until (or (null sub) (eq sub object))
               unless (ctypep client (car sub) ect)
                 return nil
               finally (return t)))))
(defmethod ctypep (client (object t) (ct clist-of))
  (declare (ignore client))
  nil)

(defmethod subctypep (client (ct1 clist-of) (ct2 clist-of))
  (subctypep client (element-ctype ct1) (element-ctype ct2)))
(defmethod ctype= (client (ct1 clist-of) (ct2 clist-of))
  (ctype= client (element-ctype ct1) (element-ctype ct2)))

(defmethod disjointp (client (ct1 clist-of) (ct2 clist-of))
  (disjointp client (element-ctype ct1) (element-ctype ct2)))
(defmethod conjointp (client (ct1 clist-of) (ct2 clist-of))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct clist-of))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 clist-of) (ct2 clist-of))
  (clist-of (conjoin client (element-ctype ct1) (element-ctype ct2))))
(defmethod disjoin/2 ((ct1 clist-of) (ct2 clist-of))
  (clist-of (disjoin client (element-ctype ct1) (element-ctype ct2))))

(defmethod subtract (client (ct1 clist-of) (ct2 clist-of))
  (clist-of (conjoin client (element-ctype ct1) (negate client (element-ctype ct2)))))

;;;

(defmethod subctypep (client (ct1 clist-of) (ct2 ccons))
  (declare (ignore client))
  ;; clist-of includes nil, a non-cons
  (values nil t))
(defmethod subctypep (client (ct1 ccons) (ct2 clist-of))
  ;; cons types are never recursive, so they can't be subtype of clist-of
  ;; except in the degenerate cons type case.
  ;; (Note that we don't need to worry about degenerate clist-of,
  ;;  since (clist-of nil) is not nil, it's null.)
  (or/tri (subctypep client (ccons-car ct1) (bot))
          (subctypep client (ccons-cdr ct1) (bot))))

(defmethod subctypep (client (ct1 cmember) (ct2 clist-of))
  (declare (ignore client))
  (values (equal (cmember-members ct1) '(nil)) t))

(define-commutative-method disjointp (client (clist-of clist-of) (ccons ccons))
  (or/tri (disjointp client (ccons-car ccons) (element-ctype clist-of))
          (disjointp client (ccons-cdr ccons) clist-of)))

(define-commutative-method disjointp (client (clist-of clist-of) (cmember cmember))
  (declare (ignore client))
  (values (not (member nil (cmember-members cmember))) t))

(defexclusives clist-of range ccomplex carray charset cfunction fpzero)

(define-commutative-method conjointp (client (ct1 clist-of) (ct2 cclass))
  (declare (ignore client))
  (values nil t))

;;; LIST is a subtype of SEQUENCE, so all CLIST-OF types are as well.
(defun sequence-cclass-p (cclass)
  (eq (class-name (cclass-class cclass)) 'sequence))
(defmethod subctypep (client (ct1 clist-of) (ct2 cclass))
  (declare (ignore client))
  (values (sequence-cclass-p ct2) t))
(defmethod subctypep (client (ct1 cclass) (ct2 clist-of))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 clist-of) (ct2 cclass))
  (declare (ignore client))
  (values (not (sequence-cclass-p ct2)) t))
(define-commutative-method conjoin/2 (client (ct1 clist-of) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) ct1 (bot)))
(define-commutative-method disjoin/2 (client (ct1 clist-of) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) ct2 nil))
(defmethod subtract (client (ct1 clist-of) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract (client (ct1 cclass) (ct2 clist-of))
  (declare (ignore client))
  (if (sequence-cclass-p ct1) nil (bot)))
