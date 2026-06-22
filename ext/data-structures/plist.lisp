(in-package #:ctype.ext.data-structures)

(defclass cproperty-list (ctype)
  ((%key-ctypes :initarg :key-ctypes :reader key-ctypes :type list))
  (:documentation "The type of property lists where values of the given keys must be of the corresponding types. Other properties are allowed but ignored. If one of the given keys is not present, its value is considered `nil'."))

(defmethod property-ctype ((plist cproperty-list) property-key)
  (or (cdr (assoc property-key (key-ctypes plist))) (top)))

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

(defmethod cons-specifier-ctype ((client client) (head (eql 'plist))
                                 rest env)
  (destructuring-bind (&rest property-ctypes &key &allow-other-keys) rest
    (cproperty-list
     (loop for (key type) on property-ctypes
           collect (cons key (specifier-ctype client type env))))))

(defmethod ctypep (client (list list) (ct cproperty-list))
  (and (let ((len (list-length list))) (and len (evenp len)))
       (loop for (key . type) in (key-ctypes ct)
             always (ctypep client (getf list key) type))))
(defmethod ctypep (client (object t) (ct cproperty-list))
  (declare (ignore client))
  nil)

(defmethod subctypep (client (ct1 cproperty-list) (ct2 cproperty-list))
  (values
   (loop for (key . type2) in (key-ctypes ct2)
         always
         (subctypep client (property-ctype ct1 key) type2))
   t))

(defmethod disjointp (client (ct1 cproperty-list) (ct2 cproperty-list))
  (some/tri (lambda (key)
              (disjointp client
                         (property-ctype ct1 key) (property-ctype ct2 key)))
            (union (keys ct1) (keys ct2))))

(defmethod conjointp (client (ct1 cproperty-list) (ct2 cproperty-list))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct cproperty-list))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 cproperty-list) (ct2 cproperty-list))
  (cproperty-list
   (loop for key in (union (keys ct1) (keys ct2))
         for con = (conjoin client (property-ctype ct1 key)
                            (property-ctype ct2 key))
         if (bot-p con)
           do (return-from conjoin/2 con)
         else collect (cons key con))))

(defmethod disjoin/2 (client (ct1 cproperty-list) (ct2 cproperty-list))
  (cproperty-list
   (loop for key in (union (keys ct1) (keys ct2))
         for dis = (disjoin client
                            (property-ctype ct1 key) (property-ctype ct2 key))
         unless (top-p dis)
           collect (cons key dis))))

(defmethod subtract (client (ct1 cproperty-list) (ct2 cproperty-list))
  (cproperty-list
   (loop for key in (union (keys ct1) (keys ct2))
         for con = (conjoin client (property-ctype ct1 key)
                            (negate client (property-ctype ct2 key)))
         if (bot-p con)
           do (return-from subtract con)
         else collect (cons key con))))

;;;

(defexclusives cproperty-list range ccomplex carray charset cfunction fpzero)

(defun sequence-cclass-p (cclass)
  (eq (class-name (cclass-class cclass)) 'sequence))
(defmethod subctypep (client (ct1 cproperty-list) (ct2 cclass))
  (declare (ignore client))
  (values (sequence-cclass-p ct2) t))
(defmethod subctypep (client (ct1 cclass) (ct2 cproperty-list))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 cproperty-list) (ct2 cclass))
  (declare (ignore client))
  (values (not (sequence-cclass-p ct2)) t))
(define-commutative-method conjoin/2 (client (ct1 cproperty-list) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) ct1 (bot)))
(define-commutative-method disjoin/2 (client (ct1 cproperty-list) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) ct2 nil))
(defmethod subtract (client (ct1 cproperty-list) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract (client (ct1 cclass) (ct2 cproperty-list))
  (declare (ignore client))
  (if (sequence-cclass-p ct1) nil (bot)))

(defmethod subctypep (client (plist cproperty-list) (list-of clist-of))
  (declare (ignore client))
  (if (top-p (element-ctype list-of))
      (values t t)
      (values nil nil)))
