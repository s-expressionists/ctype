(in-package #:ctype)

(defmethod ctypep (object (ct negation))
  (not (ctypep object (negation-ctype ct))))

(defmethod subctypep ((ct1 negation) (ct2 negation))
  (subctypep (negation-ctype ct2) (negation-ctype ct1)))
;;; If A is a subset of B, A is not a subset of ~B. Unless A is bottom.
;;; However if A is not a subset of B it does not follow that A <: ~B;
;;; for example if A and B overlap.
(defmethod subctypep ((ct1 ctype) (ct2 negation))
  (let ((neg (negation-ctype ct2)))
    (cond ((bot-p ct1) (values t t))
          ((subctypep ct1 neg) (values nil t))
          ((disjointp ct1 neg) (values t t))
          (t (call-next-method)))))
;;; Similar to above.
(defmethod subctypep ((ct1 negation) (ct2 ctype))
  (cond ((top-p ct2) (values t t))
        ((subctypep (negation-ctype ct1) ct2) (values nil t))
        (t (call-next-method))))

(defmethod negate ((ctype negation)) (negation-ctype ctype))

(defmethod conjoin/2 ((ct1 negation) (ct2 negation))
  (let ((nt1 (negation-ctype ct1)) (nt2 (negation-ctype ct2)))
    ;; a <: b => ~b <: ~a => ~b ^ ~a = ~b
    (cond ((subctypep nt1 nt2) ct2)
          ((subctypep nt2 nt1) ct1)
          (t
           ;; Give de Morgan a shot.
           (let ((p (disjoin/2 nt1 nt2)))
             (if p
                 (negate p)
                 (call-next-method)))))))
(defmethod conjoin/2 ((ct1 negation) (ct2 ctype))
  (or (subtract ct2 (negation-ctype ct1)) (call-next-method)))
(defmethod conjoin/2 ((ct1 ctype) (ct2 negation))
  (or (subtract ct1 (negation-ctype ct2)) (call-next-method)))

(defmethod disjoin/2 ((ct1 negation) (ct2 negation))
  (let ((nt1 (negation-ctype ct1)) (nt2 (negation-ctype ct2)))
    ;; a <: b => ~b <: ~a => ~b v ~a = ~a
    (cond ((subctypep nt1 nt2) ct1)
          ((subctypep nt2 nt1) ct2)
          (t (let ((p (conjoin/2 nt1 nt2)))
               (if p
                   (negate p)
                   (call-next-method)))))))
(defmethod disjoin/2 ((ct1 negation) (ct2 ctype))
  (if (subctypep (negation-ctype ct1) ct2)
      (top)
      (call-next-method)))
(defmethod disjoin/2 ((ct1 ctype) (ct2 negation))
  (if (subctypep (negation-ctype ct2) ct1)
      (top)
      (call-next-method)))

(defmethod subtract ((ct1 ctype) (ct2 negation))
  (conjoin/2 ct1 (negation-ctype ct2)))

(defmethod unparse ((ct negation))
  `(not ,(unparse (negation-ctype ct))))
