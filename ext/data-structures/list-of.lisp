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

(defmethod ctypep ((object null) (ct clist-of)) t)
(defmethod ctypep ((object cons) (ct clist-of))
  (let ((ect (element-ctype ct)))
    (and (ctypep (car object) ect)
         ;; Traverse circular lists carefully
         (loop for sub = (cdr object) then (cdr sub)
               until (or (null sub) (eq sub object))
               unless (ctypep (car sub) ect)
                 return nil
               finally (return t)))))
(defmethod ctypep ((object t) (ct clist-of)) nil)

(defmethod subctypep ((ct1 clist-of) (ct2 clist-of))
  (subctypep (element-ctype ct1) (element-ctype ct2)))
(defmethod ctype= ((ct1 clist-of) (ct2 clist-of))
  (ctype= (element-ctype ct1) (element-ctype ct2)))

(defmethod disjointp ((ct1 clist-of) (ct2 clist-of))
  (disjointp (element-ctype ct1) (element-ctype ct2)))
(defmethod conjointp ((ct1 clist-of) (ct2 clist-of)) (values nil t))

(defmethod cofinitep ((ct clist-of)) (values nil t))

(defmethod conjoin/2 ((ct1 clist-of) (ct2 clist-of))
  (clist-of (conjoin (element-ctype ct1) (element-ctype ct2))))
(defmethod disjoin/2 ((ct1 clist-of) (ct2 clist-of))
  (clist-of (disjoin (element-ctype ct1) (element-ctype ct2))))

(defmethod subtract ((ct1 clist-of) (ct2 clist-of))
  (clist-of (conjoin (element-ctype ct1) (negate (element-ctype ct2)))))

;;;

(defmethod subctypep ((ct1 clist-of) (ct2 ccons))
  ;; clist-of includes nil, a non-cons
  (values nil t))
(defmethod subctypep ((ct1 ccons) (ct2 clist-of))
  (let ((element-type (element-ctype ct2)))
    (do ((ct1 ct1 (ccons-cdr ct1)))
        ((ctype= (cmember nil) ct1)
         (values t t))
      (let ((type (ccons-car ct1)))
        (unless (subctypep type element-type)
          (return-from subctypep (values nil t)))))))

(defmethod subctypep ((ct1 cmember) (ct2 clist-of))
  (values (equal (cmember-members ct1) '(nil)) t))

(define-commutative-method disjointp ((clist-of clist-of) (ccons ccons))
  (or/tri (disjointp (ccons-car ccons) (element-ctype clist-of))
          (disjointp (ccons-cdr ccons) clist-of)))

(define-commutative-method disjointp ((clist-of clist-of) (cmember cmember))
  (values (not (member nil (cmember-members cmember))) t))

(defexclusives clist-of range ccomplex carray charset cfunction fpzero)

(define-commutative-method conjointp ((ct1 clist-of) (ct2 cclass))
  (values nil t))

;;; LIST is a subtype of SEQUENCE, so all CLIST-OF types are as well.
(defun sequence-cclass-p (cclass)
  (eq (class-name (cclass-class cclass)) 'sequence))
(defmethod subctypep ((ct1 clist-of) (ct2 cclass))
  (values (sequence-cclass-p ct2) t))
(defmethod subctypep ((ct1 cclass) (ct2 clist-of)) (values nil t))
(define-commutative-method disjointp ((ct1 clist-of) (ct2 cclass))
  (values (not (sequence-cclass-p ct2)) t))
(define-commutative-method conjoin/2 ((ct1 clist-of) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct1 (bot)))
(define-commutative-method disjoin/2 ((ct1 clist-of) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct2 nil))
(defmethod subtract ((ct1 clist-of) (ct2 cclass))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract ((ct1 cclass) (ct2 clist-of))
  (if (sequence-cclass-p ct1) nil (bot)))
