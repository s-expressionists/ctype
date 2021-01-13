(in-package #:ctype)

(defgeneric ctypep (object ctype)
  (:argument-precedence-order ctype object))

;;; To avoid infinite recursion, subctypep must not call negate, conjoin/2, or
;;; disjoin/2, and so those functions may call subctypep all they want.
(defgeneric subctypep (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype)) (values nil nil)))

(defgeneric negate (ctype)
  (:method ((ctype ctype)) (negation ctype)))

;;; These two return NIL if no special simplification is possible;
;;; CONJOIN and DISJOIN will then make a conjunction/disjunction ctype.
(defgeneric conjoin/2 (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype))
    (cond ((subctypep ct1 ct2) ct1)
          ((subctypep ct2 ct1) ct2)
          (t nil))))
(defgeneric disjoin/2 (ctype1 ctype2)
  (:method ((ct1 ctype) (ct2 ctype))
    (cond ((subctypep ct1 ct2) ct2)
          ((subctypep ct2 ct1) ct1)
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
                do (loop for (ctype2 . more) on rest
                         for j = (,simp ctype1 ctype2)
                         when j
                           do (return-from ,name
                                (apply #',name j
                                       (append unsimplified more))))
                collect ctype1 into unsimplified
                finally (return (apply #',junct unsimplified))))))
  (defjoin conjoin conjoin/2 conjunction)
  (defjoin disjoin disjoin/2 disjunction))
