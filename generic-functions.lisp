(in-package #:ctype)

(defgeneric ctypep (object ctype)
  (:argument-precedence-order ctype object))

;;; To avoid infinite recursion, subctypep must not call negate, conjoin/2, or
;;; disjoin/2, and so those functions may call subctypep all they want.
;;; When subctypep or disjointp call themselves or each other, it must be
;;; ensured that the problem is simpler, e.g. by dropping negations.
(defgeneric subctypep (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype)) (values nil nil)))

;;; Optional wrapper to speed up subctypep and make usage clearer.
(defgeneric ctype= (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype))
    (and/tri (subctypep ct1 ct2) (subctypep ct2 ct1))))

;;; Is the conjunction of these types bottom?
;;; Ditto the restrictions on calling negate etc.
(defgeneric disjointp (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype)) (values nil nil)))
;;; Dual to disjointp: Is the disjunction of these types top?
(defgeneric conjointp (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype)) (values nil nil)))

;;; Ditto the restrictions etc., and returns the same kinds of values.
;;; Determines whether the negation of a type is finite. This is used to
;;; resolve questions like (subtypep '(not X) '(member ...))
(defgeneric cofinitep (ctype)
  (:method ((ct ctype)) (values nil nil)))

(defgeneric negate (ctype)
  (:method ((ctype ctype)) (negation ctype)))

;;; These two return NIL if no special simplification is possible;
;;; CONJOIN and DISJOIN will then make a conjunction/disjunction ctype.
(defgeneric conjoin/2 (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype))
    (cond ((disjointp ct1 ct2) (bot))
          ((subctypep ct1 ct2) ct1)
          ((subctypep ct2 ct1) ct2)
          (t nil))))
(defgeneric disjoin/2 (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype))
    (cond ((conjointp ct1 ct2) (top)) ; for completeness more than practicality
          ((subctypep ct1 ct2) ct2)
          ((subctypep ct2 ct1) ct1)
          (t nil))))

;;; Simplifier for (conjoin/2 ct1 (negate ct2)).
;;; Like the /2, returns NIL if no simplification is apparent.
(defgeneric subtract (ctype1 ctype2)
  (:argument-precedence-order ctype2 ctype1)
  (:method ((ct1 ctype) (ct2 ctype))
    (cond ((disjointp ct1 ct2) ct1)
          ((subctypep ct1 ct2) (bot))
          (t nil))))

(defgeneric unparse (ctype))

(defmethod print-object ((ct ctype) stream)
  (print-unreadable-object (ct stream :type t)
    (write (unparse ct) :stream stream)))

(macrolet
    ((defjoin (name simp junct)
       `(defun ,name (&rest ctypes)
          ;; If any pairwise junctions are simplifiable, recurse with that.
          ;; Otherwise dump into a junction type.
          (loop for (ctype1 . rest) on ctypes
                do (loop for ctype2 in rest
                         for j = (,simp ctype1 ctype2)
                         when j
                           do (return-from ,name
                                (apply #',name
                                       (append (substitute j ctype2 rest
                                                           :count 1)
                                               unsimplified))))
                collect ctype1 into unsimplified
                finally (return (apply #',junct unsimplified))))))
  (defjoin conjoin conjoin/2 conjunction)
  (defjoin disjoin disjoin/2 disjunction))
