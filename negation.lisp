(in-package #:ctype)

(defmethod ctypep (object (ct negation))
  (not (ctypep object (negation-ctype ct))))

;;; Negation identities:
;;; a ^ ~a = 0
;;; a v ~a = T
;;; ~~a = a

;;; a = a v 0 = a v (b ^ ~b) = (a v b) ^ (a v ~b)
;;; 0 = a ^ 0 = a ^ (b ^ ~b) = (a ^ b) ^ (a ^ ~b)
;;; a = a ^ T = a ^ (b v ~b) = (a ^ b) v (a ^ ~b)
;;; T = a v T = a v (b v ~b) = (a v b) v (a v ~b)

(defmethod subctypep ((ct1 negation) (ct2 negation))
  ;; ~a <: ~b <=> ~a ^ ~b = ~a <=> ~(a v b) = ~a <=> a v b = a <=> b <: a
  (surely (subctypep (negation-ctype ct2) (negation-ctype ct1))
          (call-next-method)))
(defmethod subctypep ((ct1 ctype) (ct2 negation))
  ;; a ^ b = 0 => 0 v (a ^ ~b) = a <=> a ^ ~b = a <=> a <: ~b
  ;; a <: ~b <=> a ^ ~b = a => (a ^ b) ^ a = 0 <=> a ^ b = 0
  ;; therefore, a ^ b = 0 <=> a <: ~b
  (surely (disjointp ct1 (negation-ctype ct2)) (call-next-method)))

(defmethod ctype= ((ct1 negation) (ct2 negation))
  (surely (ctype= (negation-ctype ct1) (negation-ctype ct2))
          (call-next-method)))

(defmethod disjointp ((ct1 negation) (ct2 ctype))
  (surely (subctypep ct2 (negation-ctype ct1)) (call-next-method)))
(defmethod disjointp ((ct1 ctype) (ct2 negation))
  (surely (subctypep ct1 (negation-ctype ct2)) (call-next-method)))

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
  (or (conjoin/2 ct1 (negation-ctype ct2)) (call-next-method)))

(defmethod unparse ((ct negation))
  (let ((up (unparse (negation-ctype ct))))
    (if (eq up 'cons)
        'atom
        `(not ,up))))
