(in-package #:ctype.ext.data-structures)

(defclass cproperty-list (ctype)
  ((%key-ctypes :initarg :key-ctypes :reader key-ctypes :type list))
  (:documentation
   "Property list where each can can be of a different type. Other
   properties are allowed. Absent properties are considered to be
   `nil'."))

(defmethod property-ctype ((plist cproperty-list) property-key)
  (or (cdr (assoc key (key-ctypes property-key)))
      (ctype:top)))

(defmethod keys ((plist cproperty-list))
  (mapcar #'car (key-ctypes plist)))

(defmethod unparse ((object cproperty-list))
  (with-accessors ((key-ctypes key-ctypes)) object
    (if key-ctypes
        `(plist ,@(loop for (key . type) in key-ctypes
                        collect key
                        collect (unparse type)))
        'plist)))

(defun cproperty-list (key-ctypes)
  (make-instance
   'cproperty-list
   :key-ctypes key-ctypes))

(define-extended-type plist (&rest property-ctypes &environment env)
  :documentation
  "A property list where the `second' of each PROPERTY-CTYPE is the type of
  the `first' key of each PROPERTY-CTYPE."
  :simple ((declare (ignore property-ctypes env))
           'list)
  :extended
  ((cproperty-list
    (loop for (key type) on property-ctypes by #'cddr
          collect (cons key (extended-specifier-ctype type env))))))

(defmethod ctypep ((list list) (ct cproperty-list))
  (and (evenp (length list))
       (loop for (key . type) in (key-ctypes ct)
             always
             (ctypep (getf list key) type))))
(defmethod ctypep ((object t) (ct cproperty-list)) nil)

(defmethod subctypep ((ct1 cproperty-list) (ct2 cproperty-list))
  (values
   (loop for (key . type2) in (key-ctypes ct2)
         always
         (subctypep (property-ctype ct1 key) type2))
   t))

(defmethod disjointp ((ct1 cproperty-list) (ct2 cproperty-list))
  (let (total-disjointp (total-surep t))
    (loop for key in (union (keys ct1) (keys ct2)) do
          (multiple-value-bind (disjointp surep) (disjointp (property-ctype ct1 key) (property-ctype ct2 key))
            (setq total-disjointp (or total-disjointp disjointp)
                  total-surep (and total-surep surep))))
    (values total-disjointp total-surep)))

(defmethod conjointp ((ct1 cproperty-list) (ct2 cproperty-list)) (values nil t))

(defmethod cofinitep ((ct cproperty-list)) (values nil t))

(defmethod conjoin/2 ((ct1 cproperty-list) (ct2 cproperty-list))
  (cproperty-list
   (loop for key in (union (keys ct1) (keys ct2))
         collect
         (let ((type (conjoin (property-ctype ct1 key)
                              (property-ctype ct2 key))))
           (if (bot-p type)
               (return-from conjoin/2 (ctype:bot))
               (cons key type))))))

(defmethod disjoin/2 ((ct1 cproperty-list) (ct2 cproperty-list))
  (cproperty-list
   (loop for key in (union (keys ct1) (keys ct2))
         collect
         (cons key (disjoin (property-ctype ct1 key)
                            (property-ctype ct2 key))))))

(defmethod subtract ((ct1 cproperty-list) (ct2 cproperty-list))
  (cproperty-list
   (loop for key in (union (keys ct1) (keys ct2))
         collect
         (cons key (conjoin (property-ctype ct1 key)
                            (negate (property-ctype ct2 key)))))))

;;;

(defmethod subctypep ((ct1 cproperty-list) (ct2 ccons))
  (values
   (and
    (top-p (ccons-car ct2))
    (top-p (ccons-cdr ct2))
    (loop for (key . type) in (key-ctypes ct1)
          never
          (subctypep (cmember nil) type)))
   t))

(defmethod subctypep ((ct1 ccons) (ct2 cproperty-list))
  (let ((not-present-keys (keys ct2)))
    (do ((ct1 ct1 (ccons-cdr (ccons-cdr ct1))))
        ((ctype= (cmember nil) ct1))
      (when (ctype= (cmember nil) (ccons-cdr ct1))
        (return-from subctypep (values nil t)))
      (let ((key-spot (ccons-car ct1))
            (type (ccons-car (ccons-cdr ct1))))
        (if (typep key-spot 'cmember)
            (dolist (key (cmember-members key-spot))
              (setq not-present-keys (delete key not-present-keys))
              (unless (subctypep type (property-ctype ct2 key))
                (return-from subctypep (values nil t))))
            (return-from subctypep (values nil nil)))))
    (dolist (key not-present-keys)
      (unless (subctypep (cmember nil) (property-ctype ct2 key))
        (return-from subctypep (values nil t))))
    (values t t)))

(defmethod subctypep ((ct1 cmember) (ct2 cproperty-list))
  (values
   (and (equal (cmember-members ct1) '(nil))
        (every
         (lambda (property-ctype)
           (subctypep (cmember nil) (cdr property-ctype)))
         (key-ctypes ct2)))
   t))

(define-commutative-method disjointp ((plist cproperty-list) (ccons ccons))
  (multiple-value-bind (subctypep surep)
      (or/tri (subctypep plist ccons)
              (subctypep ccons plist))
    (values (not subctypep) surep)))

(define-commutative-method disjointp ((plist cproperty-list) (cmember cmember))
  (values (not (and (member nil (cmember-members cmember))
                    (ctypep nil plist)))
          t))

(defexclusives cproperty-list range ccomplex carray charset cfunction fpzero)

(defun sequence-cclass-p (cclass)
  (eq (class-name (cclass-class cclass)) 'sequence))
(defmethod subctypep ((ct1 cproperty-list) (ct2 cclass))
  (values (sequence-cclass-p ct2) t))
(defmethod subctypep ((ct1 cclass) (ct2 cproperty-list)) (values nil t))
(define-commutative-method disjointp ((ct1 cproperty-list) (ct2 cclass))
  (values (not (sequence-cclass-p ct2)) t))
(define-commutative-method conjoin/2 ((ct1 cproperty-list) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct1 (bot)))
(define-commutative-method disjoin/2 ((ct1 cproperty-list) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct2 nil))
(defmethod subtract ((ct1 cproperty-list) (ct2 cclass))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract ((ct1 cclass) (ct2 cproperty-list))
  (if (sequence-cclass-p ct1) nil (bot)))

(defmethod subctypep ((plist cproperty-list) (list-of clist-of))
  (if (top-p (element-ctype list-of))
      (values t t)
      (values nil t)))
