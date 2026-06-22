(in-package #:ctype)

(defmethod ctypep (client (object array) (ct carray))
  (let ((uaet (carray-uaet ct))
        (dims (carray-dims ct))
        (simplicity (carray-simplicity ct)))
    (and (if (eq simplicity :simple)
             (simple-array-p client object)
             (not (simple-array-p client object)))
         (or (eq uaet '*)
             (equal uaet (array-element-type object)))
         (or (eq dims '*)
             (let ((rank (length dims)))
               (and (= (array-rank object) rank)
                    (loop for i from 0 below rank
                          for dim in dims
                          always (or (eq dim '*)
                                     (= (array-dimension object i) dim)))))))))
(defmethod ctypep (client (object t) (ct carray))
  (declare (ignore client))
  nil)

(defmethod subctypep (client (ct1 carray) (ct2 carray))
  (declare (ignore client))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (simplicity1 (carray-simplicity ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2))
        (simplicity2 (carray-simplicity ct2)))
    (values
     (and (eq simplicity1 simplicity2)
          (or (eq uaet2 '*)
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

(defmethod disjointp (client (ct1 carray) (ct2 carray))
  (declare (ignore client))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (simplicity1 (carray-simplicity ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2))
        (simplicity2 (carray-simplicity ct2)))
    (values (or (not (eq simplicity1 simplicity2))
                (and (not (eq uaet1 '*)) (not (eq uaet2 '*))
                     (not (equal uaet1 uaet2)))
                (and (not (eq dims1 '*)) (not (eq dims2 '*))
                     (or (/= (length dims1) (length dims2))
                         (loop for dim1 in dims1
                               for dim2 in dims2
                               never (or (eq dim1 '*) (eq dim2 '*)
                                         (unless (= dim1 dim2) (return t)))))))
            t)))
(defmethod conjointp (client (ct1 carray) (ct2 carray))
  (declare (ignore client))
  (values nil t))

(defmethod ctype= (client (ct1 carray) (ct2 carray))
  (declare (ignore client))
  (values (and (eq (carray-simplicity ct1) (carray-simplicity ct2))
               (equal (carray-uaet ct1) (carray-uaet ct2))
               (equal (carray-dims ct1) (carray-dims ct2)))
          t))

(defmethod cofinitep (client (ct carray))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 carray) (ct2 carray))
  (let ((uaet1 (carray-uaet ct1)) (eaet1 (carray-eaet ct1))
        (dims1 (carray-dims ct1)) (simplicity1 (carray-simplicity ct1))
        (uaet2 (carray-uaet ct2)) (eaet2 (carray-eaet ct2))
        (dims2 (carray-dims ct2)) (simplicity2 (carray-simplicity ct2)))
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
          (new-eaet (conjoin client eaet1 eaet2)))
      (carray new-simplicity new-uaet new-eaet new-dims))))

(defmethod subtract (client (ct1 carray) (ct2 carray))
  (declare (ignore client))
  (let ((uaet1 (carray-uaet ct1)) (dims1 (carray-dims ct1))
        (simplicity1 (carray-simplicity ct1))
        (uaet2 (carray-uaet ct2)) (dims2 (carray-dims ct2))
        (simplicity2 (carray-simplicity ct2)))
    ;; Since we don't really keep track of array types with chunks taken out
    ;; of them, our goal here is just to reduce things to bottom or ct1.
    (cond ((not (eq simplicity1 simplicity2)) ct1)
          ((or (eq uaet2 '*) (equal uaet1 uaet2))
           (cond ((eq dims2 '*) (bot)) ; all dimension forbidden
                 ((eq dims1 '*) nil) ; can't remove chunks
                 ((/= (length dims1) (length dims2))
                  ct1) ; different rank, dis joint
                 ((loop for dim1 in dims1
                        for dim2 in dims2
                        always (or (eq dim2 '*)
                                   (and (not (eq dim1 '*))
                                        (or (= dim1 dim2)
                                            ;; dim mismatch
                                            (return-from subtract ct1)))))
                  ;; ct2's dimensions are a superset of ct1's
                  (bot))
                 (t ; too complicated; e.g. for dims (3 *) - (* 3)
                  nil)))
          ;; If we have (array * ...) - (array something ...), we can ignore
          ;; the subtrahend if it's of a distinct rank.
          ((eq uaet1 '*)
           (if (and (not (eq dims1 '*)) (not (eq dims2 '*))
                    (/= (length dims1) (length dims2)))
               ct1
               nil))
          ;; Distinct uaets
          (t ct1))))

(defun unparse-vector-simple (uaet length)
  (let* ((front (case uaet
                  ((bit) '(simple-bit-vector))
                  ((base-char) '(simple-base-string))
                  ((t) '(simple-vector))
                  (otherwise (return-from unparse-vector-simple nil))))
         (back (if (eq length '*)
                   nil
                   (list length)))
         (all (append front back)))
    (if (= (length all) 1)
        (first all)
        all)))

(defmethod unparse ((ct carray))
  (let* ((uaet (carray-uaet ct)) (dims (carray-dims ct))
         (tail (if (eq dims '*)
                   (if (eq uaet '*)
                       nil
                       `(,uaet))
                   `(,uaet ,dims))))
    (if (eq (carray-simplicity ct) :simple)
        (cond ((null tail) 'simple-array)
              ((and (not (eq dims '*))
                    (= (length dims) 1)
                    (unparse-vector-simple uaet (first dims))))
              (t `(simple-array ,@tail)))
        (if (null tail)
            '(and array (not simple-array))
            `(and (array ,@tail) (not simple-array))))))
