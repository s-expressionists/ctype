(in-package #:ctype)

(defmethod ctypep ((object array) (ct carray))
  (let ((uaet (carray-uaet ct))
        (dims (carray-dims ct)))
    (and (or (eq uaet '*)
             (equal uaet (array-element-type object)))
         (or (eq dims '*)
             (and (= (array-rank object) (length dims))
                  (loop for i from 0
                        for dim in dims
                        always (or (eq dim '*)
                                   (= (array-dimension object i) dim))))))))
(defmethod ctypep ((object t) (ct carray)) nil)

(defmethod subctypep ((ct1 carray) (ct2 carray))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2)))
    (values
     (and (or (eq uaet2 '*)
              (equal uaet1 uaet2))
          (or (eq dims2 '*)
              (and (not (eq dims1 '*))
                   (= (length dims1) (length dims2))
                   (loop for dim1 in dims1
                         for dim2 in dims2
                         always (or (eq dim2 '*)
                                    (and (not (eq dim1 '*))
                                         (= dim1 dim2)))))))
     t)))

(defmethod conjoin/2 ((ct1 carray) (ct2 carray))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2)))
    (let ((new-uaet
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
                   (return-from conjoin/2 (bot))))))
      ;; Avoid consing if possible.
      (cond ((and (equal new-uaet uaet1) (equal new-dims dims1)) ct1)
            ((and (equal new-uaet uaet2) (equal new-dims dims2)) ct2)
            (t (make-instance 'carray :uaet new-uaet :dims new-dims))))))

(defmethod disjoin/2 ((ct1 carray) (ct2 carray))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2)))
    (let ((new-uaet
            (cond ((eq uaet1 '*) uaet1)
                  ((eq uaet2 '*) uaet2)
                  ((equal uaet1 uaet2) uaet1)
                  (t (return-from disjoin/2 (call-next-method)))))
          (new-dims
            (cond ((eq dims2 '*) dims2)
                  ((eq dims1 '*) dims1)
                  ((= (length dims1) (length dims2))
                   (loop for dim1 in dims1
                         for dim2 in dims2
                         collect (cond ((eq dim1 '*) dim1)
                                       ((eq dim2 '*) dim2)
                                       ((= dim1 dim2) dim1)
                                       (t (return-from disjoin/2
                                            (call-next-method))))))
                  (t (return-from disjoin/2 (call-next-method))))))
      (cond ((and (equal new-uaet uaet1) (equal new-dims dims1)) ct1)
            ((and (equal new-uaet uaet2) (equal new-dims dims2)) ct2)
            (t (make-instance 'carray :uaet new-uaet :dims new-dims))))))

(defmethod subtract ((ct1 carray) (ct2 carray))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2)))
    ;; Since we don't really keep track of array types with chunks taken out
    ;; of them, our goal here is just to reduce things to bottom or ct1.
    (cond ((not (or (eq uaet2 '*) (equal uaet1 uaet2)))
           ;; Array types of different UAET are disjoint.
           ct1)
          ((eq dims2 '*)
           ;; All dimensions are forbidden.
           (bot))
          ((eq dims1 '*)
           ;; We can't take chunks out.
           (call-next-method))
          ((/= (length dims1) (length dims2))
           ;; Array types of different rank are disjoint.
           ct1)
          ((loop for dim1 in dims1
                 for dim2 in dims2
                 always (or (eq dim2 '*)
                            (and (not (eq dim1 '*))
                                 (or (= dim1 dim2)
                                     ;; Dimension mismatch
                                     (return-from subtract ct1)))))
           ;; ct2's dimensions are a superset of ct1's.
           (bot))
          (t ; too complicated.
           (call-next-method)))))

(defmethod unparse ((ct carray))
  (let ((uaet (carray-uaet ct)) (dims (carray-dims ct)))
    (if (eq dims '*)
        (if (eq uaet '*)
            'array
            `(array ,uaet))
        `(array ,uaet ,dims))))
