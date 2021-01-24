(in-package #:ctype)

;;;; Floating point negative zeroes lead to an unfortunate special case in the
;;;; CL type system.
;;;; To review, if distinct negative zeroes exist, (= -0.0 0.0) is true, but
;;;; (eql -0.0 0.0) is false. This means that (or (eql 0.0) (float (0.0)))
;;;; cannot be reduced into a range type (or disjunction of them, whatever),
;;;; because (typep -0.0 '(or (eql 0.0) (float (0.0)))) is false whereas
;;;; (typep -0.0 '(float 0.0)) is true.
;;;; To deal with this, we have an entirely separate ctype class, fpzero.
;;;; An fpzero ctype represents an (eql floating-point-zero) type specifier.
;;;; Since the problem is mostly in relating to ranges, the important methods
;;;; for these are in pairwise.lisp, except we do sometimes form ranges here
;;;; for (or (eql -0.0) (eql 0.0)).

(defmethod ctypep (object (ctype fpzero))
  (eql object (fpzero-zero object)))

(defmethod subctypep ((ct1 fpzero) (ct2 fpzero))
  (values (eql (fpzero-zero ct1) (fpzero-zero ct2)) t))

(defmethod ctype= ((ct1 fpzero) (ct2 fpzero))
  (values (eql (fpzero-zero ct1) (fpzero-zero ct2)) t))

(defmethod disjointp ((ct1 fpzero) (ct2 fpzero))
  (values (not (eql (fpzero-zero ct1) (fpzero-zero ct2))) t))
(defmethod conjointp ((ct1 fpzero) (ct2 fpzero)) (values nil t))

(defmethod cofinitep ((ct fpzero)) (values nil t))

(defmethod conjoin/2 ((ct1 fpzero) (ct2 fpzero))
  (if (eql (fpzero-zero ct1) (fpzero-zero ct2))
      ct1
      (bot)))

(defmethod disjoin/2 ((ct1 fpzero) (ct2 fpzero))
  (let ((k1 (fpzero-kind ct1))
        (z1 (fpzero-zero ct1)) (z2 (fpzero-zero ct2)))
    (cond ((eql z1 z2) ct1)
          ;; (member -0.0 0.0): make a range
          ((eql z1 (- z2)) (range k1 z1 nil z1 nil))
          (t (call-next-method)))))

(defmethod subtract ((ct1 fpzero) (ct2 fpzero))
  (if (eql (fpzero-zero ct1) (fpzero-zero ct2))
      (bot)
      ct1))

(defmethod unparse ((ct fpzero)) `(eql ,(fpzero-zero ct)))
