(in-package #:ctype.ext.data-structures)

(defclass carray-of (carray) ()
  (:documentation "Homogeneous array ctype."))

(defun carray-of (element-ctype &optional (dims '*) (upgraded-element-type '*) simplicity)
  (if simplicity
      (make-instance
       'carray-of
       :simplicity simplicity :uaet upgraded-element-type :eaet element-ctype :dims dims)
      (let ((simple
              (make-instance
               'carray-of
               :simplicity :simple :uaet upgraded-element-type :eaet element-ctype :dims dims)))
        (if ctype:+complex-arrays-exist-p+
            (disjoin simple
                     (make-instance
                      'carray-of
                      :simplicity :complex :uaet upgraded-element-type :eaet element-ctype :dims dims))
            simple))))

(define-extended-type array-of (element-type &optional (dims '*) (upgraded-element-type '*) &environment env)
  :documentation "An array whose elements are of type ELEMENT-TYPE."
  :simple ((declare (ignore upgraded-element-type env))
           `(array ,element-type ,dims))
  :extended
  ((carray-of (extended-specifier-ctype element-type env) dims upgraded-element-type)))

(defun simple-carray-of (element-ctype &optional (dims '*) (upgraded-element-type '*))
  (make-instance
   'carray-of
   :simplicity :simple :uaet upgraded-element-type :eaet element-ctype :dims dims))

(define-extended-type simple-array-of (element-type &optional (dims '*) (upgraded-element-type '*) &environment env)
  :documentation "A simple array whose elements are of type ELEMENT-TYPE."
  :simple ((declare (ignore upgraded-element-type env))
           `(simple-array ,element-type ,dims))
  :extended
  ((simple-carray-of (extended-specifier-ctype element-type env) dims upgraded-element-type)))

(defun cvector-of (element-ctype &optional (length '*) (upgraded-element-type '*) simplicity)
  (carray-of
   element-ctype
   (if (eq length '*)
       length
       (list length))
   upgraded-element-type simplicity))

(define-extended-type vector-of (element-type &optional (length '*) (upgraded-element-type '*) &environment env)
  :documentation "A vector whose elements are of type ELEMENT-TYPE."
  :simple ((declare (ignore upgraded-element-type env))
           `(vector ,element-type ,length))
  :extended
  ((cvector-of (extended-specifier-ctype element-type env) length upgraded-element-type)))

(defun simple-cvector-of (element-ctype &optional (length '*) (upgraded-element-type '*))
  (simple-carray-of
   element-ctype
   (if (eq length '*)
       length
       (list length))
   upgraded-element-type))

(define-extended-type simple-vector-of (element-type &optional (length '*) (upgraded-element-type '*) &environment env)
  :documentation "A simple vector whose elements are of type ELEMENT-TYPE."
  :simple ((declare (ignore upgraded-element-type env))
           `(simple-vector ,element-type ,length))
  :extended
  ((simple-cvector-of (extended-specifier-ctype element-type env) length upgraded-element-type)))

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

(defmethod ctypep ((object array) (ct carray-of))
  (let ((element-ctype (carray-eaet ct))
        (dims (carray-dims ct)))
    (and (or (eq dims '*)
             (let ((rank (length dims)))
               (and (= (array-rank object) rank)
                    (loop for i from 0 below rank
                          for dim in dims
                          always (or (eq dim '*)
                                     (= (array-dimension object i) dim))))))
         (let ((all-indexes (mapcar #'iota (array-dimensions object))))
           (block check-elements-types
             (apply #'map-product
                    (lambda (&rest indexes)
                      (unless (ctypep (apply #'aref object indexes) element-ctype)
                        (return-from check-elements-types nil)))
                    all-indexes)
             t)))))
(defmethod ctypep ((object t) (ct carray-of)) nil)

(defmethod subctypep ((ct1 carray-of) (ct2 carray-of))
  (let ((element-ctype1 (carray-eaet ct1))
        (dims1 (carray-dims ct1))
        (simplicity1 (carray-simplicity ct1))
        (element-ctype2 (carray-eaet ct2))
        (dims2 (carray-dims ct2))
        (simplicity2 (carray-simplicity ct2)))
    (and/tri
     (subctypep element-ctype1 element-ctype2)
     (values
      (and (eq simplicity1 simplicity2)
           (or (eq dims2 '*)
               (and (not (eq dims1 '*))
                    (= (length dims1) (length dims2))
                    (loop for dim1 in dims1
                          for dim2 in dims2
                          always (or (eq dim2 '*)
                                     (and (not (eq dim1 '*))
                                          (= dim1 dim2)))))))
      t))))

(defmethod conjoin/2 ((array1 carray-of) (array2 carray-of))
  (let ((uaet1 (carray-uaet array1))
        (eaet1 (carray-eaet array1))
        (dims1 (carray-dims array1))
        (simplicity1 (carray-simplicity array1))
        (uaet2 (carray-uaet array2))
        (eaet2 (carray-eaet array2))
        (dims2 (carray-dims array2))
        (simplicity2 (carray-simplicity array2)))
    (let ((new-simplicity
            (cond ((eq simplicity1 :simple)
                   (unless (eq simplicity2 :simple)
                     ;; simplicity mismatch
                     (return-from conjoin/2 (bot)))
                   simplicity1)
                  ((eq simplicity1 :complex)
                   (unless (eq simplicity2 :complex)
                     (return-from conjoin/2 (bot)))
                   simplicity2)))
          (new-uaet
            (cond ((eq uaet1 '*) uaet2)
                  ((eq uaet2 '*) uaet1)
                  ((equal uaet1 uaet2) uaet1)
                  ;; UAET mismatch
                  (t (return-from conjoin/2 (bot)))))
          (new-dims
            (cond ((eq dims2 '*) dims1)
                  ((eq dims1 '*) dims2)
                  ((= (length dims1) (length dims2))
                   (loop for dim1 in dims1
                         for dim2 in dims2
                         collect (cond ((eq dim1 '*) dim2)
                                       ((eq dim2 '*) dim1)
                                       ((= dim1 dim2) dim1)
                                       ;; Dimension mismatch
                                       (t (return-from conjoin/2 (bot))))))
                  (t ;; Rank mismatch
                   (return-from conjoin/2 (bot)))))
          (new-eaet (conjoin eaet1 eaet2)))
      (if (bot-p new-eaet)
          (bot)
          (carray-of new-eaet new-dims new-uaet new-simplicity)))))

(define-commutative-method conjoin/2 ((cclass cclass) (carray carray-of))
  (if (sequence-cclass-p cclass)
      (let ((dims (carray-dims carray)))
        (cond ((eq dims '*)
               (carray-of (carray-eaet carray)
                          '(*)
                          (carray-uaet carray)
                          (carray-simplicity carray)))
              ((= (length dims) 1) carray)
              (t (bot))))
      (bot)))
