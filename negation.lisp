(in-package #:ctype)

(defmethod ctypep (client object (ct negation))
  (not (ctypep client object (negation-ctype ct))))

;;; Negation identities:
;;; a ^ ~a = 0
;;; a v ~a = T
;;; ~~a = a

;;; a = a v 0 = a v (b ^ ~b) = (a v b) ^ (a v ~b)
;;; 0 = a ^ 0 = a ^ (b ^ ~b) = (a ^ b) ^ (a ^ ~b)
;;; a = a ^ T = a ^ (b v ~b) = (a ^ b) v (a ^ ~b)
;;; T = a v T = a v (b v ~b) = (a v b) v (a v ~b)

(defmethod subctypep (client (ct1 negation) (ct2 negation))
  ;; ~a <: ~b <=> ~a ^ ~b = ~a <=> ~(a v b) = ~a <=> a v b = a <=> b <: a
  (subctypep client (negation-ctype ct2) (negation-ctype ct1)))
(defmethod subctypep (client (ct1 ctype) (ct2 negation))
  ;; a ^ b = 0 => 0 v (a ^ ~b) = a <=> a ^ ~b = a <=> a <: ~b
  ;; a <: ~b <=> a ^ ~b = a => (a ^ b) ^ a = 0 <=> a ^ b = 0
  ;; therefore, a ^ b = 0 <=> a <: ~b
  (disjointp client ct1 (negation-ctype ct2)))
(defmethod subctypep (client (ct1 negation) (ct2 ctype))
  ;; ~b <: a <=> ~b v a = a => (a v b) v a = T <=> a v b = T
  ;; a v b = T => T ^ (a v ~b) = a <=> a v ~b = a <=> ~b <: a
  ;; therefore, a v b = T <=> ~b <: a
  (conjointp client ct2 (negation-ctype ct1)))

(defmethod ctype= (client (ct1 negation) (ct2 negation))
  (ctype= client (negation-ctype ct1) (negation-ctype ct2)))

(defmethod disjointp (client (ct1 negation) (ct2 negation))
  ;; ~a ^ ~b = 0 <=> ~(a v b) = 0 <=> a v b = T
  (conjointp client (negation-ctype ct1) (negation-ctype ct2)))
(define-commutative-method disjointp (client (ct1 negation) (ct2 ctype))
  (subctypep client ct2 (negation-ctype ct1)))

(defmethod conjointp (client (ct1 negation) (ct2 negation))
  ;; ~a v ~b = T <=> ~(a ^ b) = T <=> a ^ b = 0
  (disjointp client (negation-ctype ct1) (negation-ctype ct2)))
(define-commutative-method conjointp (client (ct1 negation) (ct2 ctype))
  (subctypep client (negation-ctype ct1) ct2))

(defmethod negate (client (ctype negation))
  (declare (ignore client))
  (negation-ctype ctype))

(defmethod conjoin/2 (client (ct1 negation) (ct2 negation))
  (let ((nt1 (negation-ctype ct1)) (nt2 (negation-ctype ct2)))
    ;; a <: b => ~b <: ~a => ~b ^ ~a = ~b
    (cond ((subctypep client nt1 nt2) ct2)
          ((subctypep client nt2 nt1) ct1)
          (t
           ;; Give de Morgan a shot.
           (let ((p (disjoin/2 client nt1 nt2)))
             (if p
                 (negate client p)
                 nil))))))
(define-commutative-method conjoin/2 (client (ct1 negation) (ct2 ctype))
  (subtract client ct2 (negation-ctype ct1)))

(defmethod disjoin/2 (client (ct1 negation) (ct2 negation))
  (let ((nt1 (negation-ctype ct1)) (nt2 (negation-ctype ct2)))
    ;; a <: b => ~b <: ~a => ~b v ~a = ~a
    (cond ((subctypep client nt1 nt2) ct1)
          ((subctypep client nt2 nt1) ct2)
          (t (let ((p (conjoin/2 client nt1 nt2)))
               (if p
                   (negate client p)
                   nil))))))
(define-commutative-method disjoin/2 (client (ct1 negation) (ct2 ctype))
  (if (subctypep client (negation-ctype ct1) ct2)
      (top)
      nil))

(defmethod subtract (client (ct1 ctype) (ct2 negation))
  (conjoin/2 client ct1 (negation-ctype ct2)))

(defmethod unparse ((ct negation))
  (let ((up (unparse (negation-ctype ct))))
    (if (eq up 'cons)
        'atom
        `(not ,up))))
