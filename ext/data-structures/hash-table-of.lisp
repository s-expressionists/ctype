(in-package #:ctype.ext.data-structures)

(defclass chash-table-of (ctype)
  ((%key-type :initarg :key :reader key-ctype)
   (%value-type :initarg :value :reader value-ctype))
  (:documentation "Homogeneous hash-table ctype."))

(defmethod unparse ((object chash-table-of))
  `(hash-table-of ,(unparse (key-ctype object))
                  ,(unparse (value-ctype object))))

(defun chash-table-of (key-ctype value-ctype)
  (make-instance
   'chash-table-of
   :key key-ctype
   :value value-ctype))

(defmethod cons-specifier-ctype ((client client) (head (eql 'hash-table-of))
                                 rest env)
  (destructuring-bind (&optional (key-type '*) (value-type '*)) rest
    (chash-table-of (if (eql key-type '*)
                        (top)
                        (specifier-ctype client key-type env))
                    (if (eql value-type '*)
                        (top)
                        (specifier-ctype client value-type env)))))

(defmethod ctypep (client (object t) (ctype chash-table-of))
  (declare (ignore client))
  nil)
(defmethod ctypep (client (object hash-table) (ctype chash-table-of))
  (let ((key-ctype (key-ctype ctype))
        (value-ctype (value-ctype ctype)))
    (loop for key being each hash-key of object using (hash-value value)
          always (and (ctypep client key key-ctype)
                      (ctypep client value value-ctype)))))

(defmethod subctypep (client (ctype1 chash-table-of) (ctype2 cclass))
  (values
   (eql (ctype:find-class client 'hash-table) (cclass-class ctype2))
   t))

(defmethod subctypep (client (ctype1 chash-table-of) (ctype2 chash-table-of))
  (and/tri (subctypep client (key-ctype ctype1) (key-ctype ctype2))
           (subctypep client (value-ctype ctype1) (value-ctype ctype2))))

(defmethod ctype= (client (ctype1 chash-table-of) (ctype2 chash-table-of))
  (and/tri (ctype= client (key-ctype ctype1) (key-ctype ctype2))
           (ctype= client (value-ctype ctype1) (value-ctype ctype2))))

(defmethod disjointp (client (ctype1 chash-table-of) (ctype2 chash-table-of))
  (or/tri (disjointp client (key-ctype ctype1) (key-ctype ctype2))
          (disjointp client (value-ctype ctype1) (value-ctype ctype2))))

(defmethod cofinitep (client (ctype chash-table-of))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 chash-table-of) (ct2 chash-table-of))
  (let ((key (conjoin client (key-ctype ct1) (key-ctype ct2)))
        (value (conjoin client (value-ctype ct1) (value-ctype ct2))))
    (if (or (bot-p key) (bot-p value))
        (bot)
        (chash-table-of key value))))

(defmethod subtract (client (ct1 chash-table-of) (ct2 chash-table-of))
  (let ((key (conjoin client (key-ctype ct1) (negate client (key-ctype ct2))))
        (value (conjoin client (value-ctype ct1) (negate client (value-ctype ct2)))))
    (if (or (bot-p key) (bot-p value))
        (bot)
        (chash-table-of key value))))

(defexclusives chash-table-of range ccomplex carray charset cfunction fpzero)

(define-commutative-method conjointp (client (ct1 cclass) (ct2 chash-table-of))
  (declare (ignore client))
  (values nil t))

(defexistential chash-table-of)
