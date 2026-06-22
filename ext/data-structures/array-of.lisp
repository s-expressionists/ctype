(in-package #:ctype.ext.data-structures)

(defclass carray-of (carray) ()
  (:documentation "Homogeneous array ctype. Uses the actual type of the elements and not just the upgraded element type."))

(defun carray-of (client element-ctype
                  &optional (dims '*) (upgraded-element-type '*) simplicity)
  (if simplicity
      (make-instance
       'carray-of
       :simplicity simplicity :uaet upgraded-element-type :eaet element-ctype :dims dims)
      (let ((simple
              (make-instance
               'carray-of
               :simplicity :simple :uaet upgraded-element-type :eaet element-ctype :dims dims)))
        (if (complex-arrays-distinct-p client)
            (disjoin simple
                     (make-instance
                      'carray-of
                      :simplicity :complex :uaet upgraded-element-type :eaet element-ctype :dims dims))
            simple))))

(defmethod cons-specifier-ctype ((client client) (head (eql 'array-of)) rest env)
  (destructuring-bind (element-type
                       &optional (dims '*) (upgraded-element-type '*))
      rest
    (carray-of client (specifier-ctype client element-type env) dims
               upgraded-element-type)))

(defun simple-carray-of (client element-ctype
                         &optional (dims '*) (upgraded-element-type '*))
  (declare (ignore client))
  (make-instance
   'carray-of
    :simplicity :simple :uaet upgraded-element-type :eaet element-ctype :dims dims))

(defmethod cons-specifier-ctype ((client client) (head (eql 'simple-array-of))
                                 rest env)
  (destructuring-bind (element-type
                       &optional (dims '*) (upgraded-element-type '*))
      rest
    (simple-carray-of client
                      (specifier-ctype client element-type env)
                      dims upgraded-element-type)))

(defun cvector-of (element-ctype &optional (length '*) (upgraded-element-type '*) simplicity)
  (carray-of
   element-ctype length upgraded-element-type simplicity))

(defmethod cons-specifier-ctype ((client client) (head (eql 'vector-of))
                                 rest env)
  (destructuring-bind (element-type
                       &optional (length '*) (upgraded-element-type '*))
      rest
    (cvector-of (specifier-ctype client element-type env)
                length upgraded-element-type)))

(defun simple-cvector-of (element-ctype &optional (length '*) (upgraded-element-type '*))
  (simple-carray-of
   element-ctype (list length) upgraded-element-type))

(defmethod cons-specifier-ctype ((client client) (head (eql 'simple-vector-of))
                                 rest env)
  (destructuring-bind (element-type
                       &optional (length '*) (upgraded-element-type '*))
      rest
    (simple-cvector-of (specifier-ctype client element-type env)
                       length upgraded-element-type)))

(defun unparse-vector-simple (type length)
  (let* ((front `(simple-vector-of ,type))
         (back (if (eq length '*)
                   nil
                   (list length)))
         (all (append front back)))
    (if (= (length all) 1)
        (first all)
        all)))

(defmethod unparse ((ct carray-of))
  (let* ((element-type (unparse (carray-eaet ct)))
         (dims (carray-dims ct))
         (tail (if (eq dims '*)
                   (if (eq element-type '*)
                       nil
                       `(,element-type))
                   `(,element-type ,dims))))
    (if (eq (carray-simplicity ct) :simple)
        (cond ((null tail) 'simple-array)
              ((and (not (eq dims '*))
                    (= (length dims) 1)
                    (unparse-vector-simple element-type (first dims))))
              (t `(simple-array-of ,@tail)))
        (if (null tail)
            '(and array (not simple-array))
            `(and (array ,@tail) (not simple-array))))))

(defmethod ctypep (client (object array) (ct carray-of))
  (and/tri (call-next-method) ; check uaet, dimensions
           ;; check elements
           (values (loop with element-ctype = (carray-eaet ct)
                         for i below (array-total-size object)
                         for elem = (row-major-aref object i)
                         always (ctypep client elem element-ctype))
                   t)))
(defmethod ctypep (client (object t) (ct carray-of))
  (declare (ignore client))
  nil)

(defmethod subctypep (client (ct1 carray-of) (ct2 carray-of))
  (and/tri (call-next-method)
           (subctypep client
                      (carray-eaet ct1) (carray-eaet ct2))))

(define-commutative-method conjoin/2
    (client (array1 carray-of) (array2 ctype))
  ;; Use the next methods (on basic carrays) and copy everything into a carray-of.
  ;; carray already merges the expressed array element types.
  (let ((result (call-next-method)))
    (if (typep result 'carray)
        (make-instance 'carray
          :dims (carray-dims result) :simplicity (carray-simplicity result)
          :eaet (carray-eaet result) :uaet (carray-uaet result))
        result))) ; nil or bottom

(defmethod disjointp (client (ct1 carray-of) (ct2 carray-of))
  (or/tri (call-next-method)
          (disjointp client (carray-eaet ct1) (carray-eaet ct2))))

(defmethod ctype= (client (ct1 carray-of) (ct2 carray-of))
  (and/tri (call-next-method)
           (ctype= client (carray-eaet ct1) (carray-eaet ct2))))
