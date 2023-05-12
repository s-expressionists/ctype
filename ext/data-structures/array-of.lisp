(in-package #:ctype.ext.data-structures)

(defclass carray-of (carray) ()
  (:documentation "Homogeneous array ctype."))

(defun carray-of (element-ctype &optional (dims '*) (upgraded-element-type '*) simplicity)
  (if simplicity
      (make-instance
       'carray-of
       :simplicity simplicity :uaet upgraded-element-type :eaet element-ctype :dims dims)
      (disjoin
       (make-instance
        'carray-of
        :simplicity :simple :uaet upgraded-element-type :eaet element-ctype :dims dims)
       (make-instance
        'carray-of
        :simplicity :complex :uaet upgraded-element-type :eaet element-ctype :dims dims))))

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
  (let* ((front (case type
                  ((bit) '(simple-bit-vector))
                  ((base-char) '(simple-base-string))
                  (otherwise '(simple-vector type))))
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
              (t `(simple-array ,@tail)))
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
