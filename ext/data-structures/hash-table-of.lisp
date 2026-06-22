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

(define-extended-type hash-table-of (key-type value-type &environment env)
  :documentation "A hash-table whose keys are of type KEY-TYPE and values are of type VALUE-TYPE."
  :simple ((declare (ignore key-type value-type env))
           'hash-table)
  :extended
  ((chash-table-of (extended-specifier-ctype key-type env)
                   (extended-specifier-ctype value-type env))))

(defmethod ctypep (client (object t) (ctype chash-table-of))
  (declare (ignore client))
  nil)
(defmethod ctypep (client (object hash-table) (ctype chash-table-of))
  (let ((key-ctype (key-ctype ctype))
        (value-ctype (value-ctype ctype)))
    (loop for key being each hash-key of object using (hash-value value) do
      (unless (and (ctypep client key key-ctype)
                   (ctypep client value value-ctype))
        (return-from ctypep nil))))
  t)

(defun compare-chash-table-of (predicate combiner client ctype1 ctype2)
  (let ((key1 (key-ctype ctype1))
        (key2 (key-ctype ctype2))
        (value1 (value-ctype ctype1))
        (value2 (value-ctype ctype2)))
    (multiple-value-bind (key-comparison key-valid)
        (funcall predicate client key1 key2)
      (multiple-value-bind (value-comparison value-valid)
          (funcall predicate client value1 value2)
        (values (funcall combiner #'identity (list key-comparison value-comparison))
                (funcall combiner #'identity (list key-valid value-valid)))))))

(defmethod subctypep (client (ctype1 chash-table-of) (ctype2 cclass))
  (declare (ignore client))
  (values
   (eql (find-class 'hash-table)
        (cclass-class ctype2))
   t))

(defmethod subctypep (client (ctype1 chash-table-of) (ctype2 chash-table-of))
  (compare-chash-table-of
   #'subctypep #'every
   client ctype1 ctype2))

(defmethod ctype= (client (ctype1 chash-table-of) (ctype2 chash-table-of))
  (compare-chash-table-of
   #'ctype= #'every
   client ctype1 ctype2))

(defmethod disjointp (client (ctype1 chash-table-of) (ctype2 chash-table-of))
  (compare-chash-table-of
   #'disjointp #'some
   client ctype1 ctype2))

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

(define-commutative-method conjointp ((ct1 cclass) (ct2 chash-table-of))
  (values nil t))

(defexistential chash-table-of)
