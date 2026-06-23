(in-package #:ctype)

(defmacro define-commutative-method (name (client arg1 arg2) &body body)
  `(progn
     (defmethod ,name (,client ,arg1 ,arg2) ,@body)
     (defmethod ,name (,client ,arg2 ,arg1) ,@body)))

(defgeneric ctypep (client object ctype)
  (:argument-precedence-order client ctype object))

;;; To avoid infinite recursion, subctypep must not call negate, conjoin/2, or
;;; disjoin/2, and so those functions may call subctypep all they want.
;;; When subctypep or disjointp call themselves or each other, it must be
;;; ensured that the problem is simpler, e.g. by dropping negations.
(defgeneric subctypep (client ctype1 ctype2)
  (:method-combination basic surely)
  (:method (client (ct1 ctype) (ct2 ctype))
    (declare (ignore client))
    (values nil nil)))

;;; Optional wrapper to speed up subctypep and make usage clearer.
(defgeneric ctype= (client ctype1 ctype2)
  (:method-combination basic surely)
  (:method (client (ct1 ctype) (ct2 ctype))
    (and/tri (subctypep client ct1 ct2) (subctypep client ct2 ct1))))

;;; Is the conjunction of these types bottom?
;;; Ditto the restrictions on calling negate etc.
(defgeneric disjointp (client ctype1 ctype2)
  (:method-combination basic surely)
  (:method (client (ct1 ctype) (ct2 ctype))
    (declare (ignore client))
    (values nil nil)))
;;; Dual to disjointp: Is the disjunction of these types top?
(defgeneric conjointp (client ctype1 ctype2)
  (:method-combination basic surely)
  (:method (client (ct1 ctype) (ct2 ctype))
    (declare (ignore client))
    (values nil nil)))

;;; Same restrictions. Is a type empty or everything (bottom or top)?
;;; Used for e.g. (subtypep t 'x), and indirectly for (subtypep '(not x) nil)
;;; which would otherwise be challenging.
;;; Note that this is distinct from bot-p and top-p, which only determine whether
;;; a type is the particular NIL or T type, i.e. does not do any analysis.
;;; Note also that these methods are NOT used in a default method on subctypep
;;; above. The idea is that a ctype class should do specific subctypep processing
;;; that removes the need for those generalities, although there's no problem
;;; with specific methods using emptyp/universalp (see ccons for example).
;;; A default method also would have to ignore NIL T results even if they do mean
;;; not-subtype for the particular case.
;;; Again see ccons (consxclusive/1 in pairwise.lisp).
(defgeneric emptyp (client ctype)
  (:method-combination basic surely)
  (:method (client (ctype ctype))
    (declare (ignore client))
    (values nil nil)))
(defgeneric universalp (client ctype)
  (:method-combination basic surely)
  (:method (client (ctype ctype))
    (declare (ignore client))
    (values nil nil)))

;;; Ditto the restrictions etc., and returns the same kinds of values.
;;; Determines whether a type or its negation are finite. This is used to
;;; resolve questions like (subtypep '(not X) '(member ...))
(defgeneric finitep (client ctype)
  (:method-combination basic surely)
  (:method (client (ct ctype))
    (declare (ignore client))
    (values nil nil)))
(defgeneric cofinitep (client ctype)
  (:method-combination basic surely)
  (:method (client (ct ctype))
    (declare (ignore client))
    (values nil nil)))

(defgeneric negate (client ctype)
  (:method (client (ctype ctype))
    (declare (ignore client))
    (negation ctype)))

;;; These two return NIL if no special simplification is possible;
;;; CONJOIN and DISJOIN will then make a conjunction/disjunction ctype.
(defgeneric conjoin/2 (client ctype1 ctype2)
  (:method-combination basic or)
  (:method (client (ct1 ctype) (ct2 ctype))
    (cond ((disjointp client ct1 ct2) (bot))
          ((subctypep client ct1 ct2) ct1)
          ((subctypep client ct2 ct1) ct2)
          (t nil))))
(defgeneric disjoin/2 (client ctype1 ctype2)
  (:method-combination basic or)
  (:method (client (ct1 ctype) (ct2 ctype))
    (cond ((conjointp client ct1 ct2) (top)) ; for completeness more than practicality
          ((subctypep client ct1 ct2) ct2)
          ((subctypep client ct2 ct1) ct1)
          (t nil))))

;;; Simplifier for (conjoin/2 ct1 (negate ct2)).
;;; Like the /2, returns NIL if no simplification is apparent.
(defgeneric subtract (client ctype1 ctype2)
  (:method-combination basic or)
  (:argument-precedence-order client ctype2 ctype1)
  (:method (client (ct1 ctype) (ct2 ctype))
    (cond ((disjointp client ct1 ct2) ct1)
          ((subctypep client ct1 ct2) (bot))
          (t nil))))

(defgeneric unparse (ctype))

(defmethod print-object ((ct ctype) stream)
  (multiple-value-bind (unparse failure)
      (ignore-errors (unparse ct))
    (if failure
        (call-next-method)
        (print-unreadable-object (ct stream :type t)
          (write unparse :stream stream))))
  ct)

;;; convenience macro for the very common case of a type always existing
;;; and never being the universe.
(defmacro defexistential (class)
  `(progn
     (defmethod emptyp (client (ctype ,class))
       (declare (ignore client))
       (values nil t))
     (defmethod universalp (client (ctype ,class))
       (declare (ignore client))
       (values nil t))))

(macrolet
    ((defjoin (name simp junct)
       `(defun ,name (client &rest ctypes)
          ;; If any pairwise junctions are simplifiable, recurse with that.
          ;; Otherwise dump into a junction type.
          (loop for (ctype1 . rest) on ctypes
                do (loop for ctype2 in rest
                         for j = (,simp client ctype1 ctype2)
                         when j
                           do (return-from ,name
                                (apply #',name
                                       client
                                       (append (substitute j ctype2 rest
                                                           :count 1)
                                               unsimplified))))
                collect ctype1 into unsimplified
                finally (return (apply #',junct unsimplified))))))
  (defjoin conjoin conjoin/2 conjunction)
  (defjoin disjoin disjoin/2 disjunction))
