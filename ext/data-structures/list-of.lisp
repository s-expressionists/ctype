(in-package #:ctype.ext.data-structures)

(defclass clist-of (ctype)
  ((%etype :initarg :etype :reader element-ctype))
  (:documentation "Homogeneous list ctype."))

(defmethod unparse ((object clist-of))
  `(list-of ,(unparse (element-ctype object))))

(defun clist-of (ect)
  (if (bot-p ect)
      (cmember nil)
      (make-instance 'clist-of :etype ect)))

(defmethod cons-specifier-ctype ((client client) (head (eql 'list-of))
                                 rest env)
  (destructuring-bind (&optional (element-type '*)) rest
    (clist-of (if (eql element-type '*)
                  (top)
                  (specifier-ctype client element-type env)))))

(defmethod ctypep (client (object null) (ct clist-of))
  (declare (ignore client))
  t)
(defmethod ctypep (client (object cons) (ct clist-of))
  (loop with ect = (element-ctype ct)
        ;; traverse potentially circular or dotted lists carefully
        ;; (Floyd's tortoise and hare)
        for tortoise = object then (cdr tortoise)
        for hare = (cdr object)
          then (when (and (consp hare) (consp (cdr hare)))
                 (cddr hare))
        until (or (null tortoise) (eq tortoise hare))
        always (and (consp tortoise)
                    (ctypep client (car tortoise) ect))))
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
(defmethod disjoin/2 (client (ct1 clist-of) (ct2 clist-of))
  (clist-of (disjoin client (element-ctype ct1) (element-ctype ct2))))

(defmethod subtract (client (ct1 clist-of) (ct2 clist-of))
  (clist-of (conjoin client (element-ctype ct1) (negate client (element-ctype ct2)))))

;;;

(defmethod subctypep (client (ct1 clist-of) (ct2 ccons))
  (declare (ignore client))
  ;; clist-of includes nil, a non-cons
  (values nil t))
(defmethod subctypep (client (ct1 ccons) (ct2 clist-of))
  (let ((element-type (element-ctype ct2)))
    (do ((ct1 ct1 (ccons-cdr ct1)))
        ((ctype= client (cmember nil) ct1)
         (values t t))
      (let ((type (ccons-car ct1)))
        (unless (subctypep client type element-type)
          (return-from subctypep (values nil t)))))))

(defmethod subctypep (client (ct1 cmember) (ct2 clist-of))
  (declare (ignore client))
  (values (equal (cmember-members ct1) '(nil)) t))

(define-commutative-method disjointp (client (clist-of clist-of) (ccons ccons))
  (or/tri (disjointp client (ccons-car ccons) (element-ctype clist-of))
          (disjointp client (ccons-cdr ccons) clist-of)))

(define-commutative-method disjointp (client (clist-of clist-of) (cmember cmember))
  (declare (ignore client))
  (values (not (member nil (cmember-members cmember))) t))

(defexclusives clist-of cclass range ccomplex carray charset cfunction fpzero)

(define-commutative-method conjointp (client (ct1 clist-of) (ct2 csequence))
  (declare (ignore client))
  (values nil t))

;;; LIST is a subtype of SEQUENCE, so all CLIST-OF types are as well.
(defmethod subctypep (client (ct1 clist-of) (ct2 csequence))
  (declare (ignore client))
  (values t t))
(defmethod subctypep (client (ct1 csequence) (ct2 clist-of))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 clist-of) (ct2 csequence))
  (declare (ignore client))
  (values nil t))
(define-commutative-method conjoin/2 (client (ct1 clist-of) (ct2 csequence))
  (declare (ignore client))
  ct1)
(define-commutative-method disjoin/2 (client (ct1 clist-of) (ct2 csequence))
  (declare (ignore client))
  ct2)
(defmethod subtract (client (ct1 clist-of) (ct2 csequence))
  (declare (ignore client))
  (bot))
