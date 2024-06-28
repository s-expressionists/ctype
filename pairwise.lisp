(in-package #:ctype)

;;;; Pair methods
;;;; That is, methods on two specific ctype classes.

;;; cclass ctypes are excluded from several other ctypes (when things are
;;; normalized correctly), so we can mark their conjunctions as empty, etc.

(defmacro defexclusive/2 (class1 class2)
  `(progn
     (define-commutative-method subctypep (client (ct1 ,class1) (ct2 ,class2))
       (declare (ignore client))
       (values nil t))
     (define-commutative-method disjointp (client (ct1 ,class1) (ct2 ,class2))
       (declare (ignore client))
       (values t t))
     (define-commutative-method conjointp (client (ct1 ,class1) (ct2 ,class2))
       (declare (ignore client))
       (values nil t))))

(defmacro defexclusive (&rest classes)
  `(progn
     ,@(loop for (class1 . rest) on classes
             nconc (loop for class2 in rest
                         collect `(defexclusive/2 ,class1 ,class2)))))

(defmacro defexclusives (main &rest classes)
  `(progn ,@(loop for class in classes
                  collect `(defexclusive ,main ,class))))

(defexclusive range ccomplex carray charset cfunction)
(defexclusives cclass range ccomplex charset)
(defexclusives fpzero cmember ccomplex carray charset cfunction)

;;; cons types are unfortunately ambiguous: (cons (satisfies foo)) MIGHT be
;;; bottom "in disguise", and might not be.
;;; This is basically (subctypep ccons (bot)), but we use this in subctypep
;;; methods themselves, so we have to reduce it to mimic the usual subctypep
;;; method in ccons.lisp.
;;; To be clear, the idea here is not that a subctypep will ever return true-
;;; thanks to normalization, it won't. But whether the cons is definitely NOT
;;; bottom can vary.
(defun ccons-bottom-p (client ccons)
  (or/tri (subctypep client (ccons-car ccons) (bot))
          (subctypep client (ccons-cdr ccons) (bot))))

(defmethod subctypep (client (ct1 ccons) (ct2 ctype))
  (if (ccons-bottom-p client ct1) (values t t) (values nil nil)))

(macrolet ((consxclusive/1 (class)
             `(progn
                (defmethod subctypep (client (ct1 ccons) (ct2 ,class))
                  ;; ct1 and ct2 are basically exclusive, so if ct1 is
                  ;; definitely NOT bottom, they really are exclusive.
                  ;; That's why this is different from the general method above.
                  (ccons-bottom-p client ct1))
                (defmethod subctypep (client (ct1 ,class) (ct2 ccons))
                  (declare (ignore client))
                  (values nil t))
                (define-commutative-method disjointp
                    (client (ct1 ccons) (ct2 ,class))
                  (declare (ignore client))
                  (values t t))))
           (consxclusive (&rest classes)
             `(progn ,@(loop for class in classes
                             collect `(consxclusive/1 ,class)))))
  (consxclusive range ccomplex carray charset cfunction fpzero))

(macrolet ((defnonconjoint/2 (c1 c2)
             `(define-commutative-method conjointp (client (ct1 ,c1) (ct2 ,c2))
                (declare (ignore client))
                (values nil t)))
           (defnonconjoint (&rest classes)
             `(progn
                ,@(loop for (class1 . rest) on classes
                        nconc (loop for class2 in rest
                                    collect `(defnonconjoint/2
                                                 ,class1 ,class2))))))
  (defnonconjoint cclass ccons range fpzero ccomplex cmember carray
    charset cfunction))

;;; Some cclass ctype relations we unfortunately have to handle specially.
(defun sequence-cclass-p (cclass)
  (eq (class-name (cclass-class cclass)) 'sequence))
;;; CONS is a subclass of SEQUENCE. Therefore, all CONS types are subtypes of
;;; SEQUENCE, regardless of whether they actually describe proper sequences,
;;; and even if they don't.
(defmethod subctypep (client (ct1 ccons) (ct2 cclass))
  (or/tri (ccons-bottom-p client ct1) (values (sequence-cclass-p ct2) t)))
(defmethod subctypep (client (ct1 cclass) (ct2 ccons))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 ccons) (ct2 cclass))
  (or/tri (ccons-bottom-p client ct1) (values (not (sequence-cclass-p ct2)) t)))
(define-commutative-method conjoin/2 (client (ct1 cclass) (ct2 ccons))
  (declare (ignore client))
  (if (sequence-cclass-p ct1) ct2 (bot)))
(define-commutative-method disjoin/2 (client (ct1 cclass) (ct2 ccons))
  (declare (ignore client))
  (if (sequence-cclass-p ct1) ct1 nil))
(defmethod subtract (client (ct1 ccons) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract (client (ct1 cclass) (ct2 ccons))
  (declare (ignore client))
  (if (sequence-cclass-p ct1) nil (bot)))
;;; NULL is (MEMBER NIL), and cmember methods should already handle things.
(defmethod subctypep (client (ct1 carray) (ct2 cclass))
  (declare (ignore client))
  (values (and (sequence-cclass-p ct2)
               (let ((dims (carray-dims ct1)))
                 (and (listp dims) (= (length dims) 1))))
          t))
(defmethod subctypep (client (ct1 cclass) (ct2 carray))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 carray) (ct2 cclass))
  (declare (ignore client))
  (values (not (and (sequence-cclass-p ct2)
                    (let ((dims (carray-dims ct1)))
                      (or (eq dims '*) (= (length dims) 1)))))
          t))
(define-commutative-method conjoin/2 (client (cclass cclass) (carray carray))
  (Declare (ignore client))
  (if (sequence-cclass-p cclass)
      (let ((dims (carray-dims carray)))
        (cond ((eq dims '*)
               (carray (carray-simplicity carray)
                       (carray-uaet carray) (carray-eaet carray)
                       '(*)))
              ((= (length dims) 1) carray)
              (t (bot))))
      (bot)))
(defmethod subtract (client (ct1 cclass) (ct2 carray))
  (declare (ignore client))
  (if (sequence-cclass-p ct1)
      (let ((dims (carray-dims ct2)))
        (if (or (eq dims '*) (= (length dims) 1))
            nil
            ct1))
      ct1))
(defmethod subtract (client (ct1 carray) (ct2 cclass))
  (declare (ignore client))
  (if (sequence-cclass-p ct2)
      (let ((dims (carray-dims ct1)))
        (cond ((eq dims '*) nil)
              ((= (length dims) 1) (bot))
              (t ct1)))
      ct1))

(defun subfunction-cclass-p (client cclass)
  ;; FIXME: We skip the env here, is that okay?
  (subclassp client (cclass-class cclass) (find-class 'function t)))
(defmethod subctypep (client (ct1 cfunction) (ct2 cclass))
  (declare (ignore client))
  ;; FUNCTION itself is never a cclass, so
  (values nil t))
(defmethod subctypep (client (ct1 cclass) (ct2 cfunction))
  (if (subfunction-cclass-p client ct1)
      (if (function-top-p ct2) (values t t) (values nil nil))
      (values nil t)))
(define-commutative-method conjoin/2 (client (ct1 cclass) (ct2 cfunction))
  (if (subfunction-cclass-p client ct1)
      (if (function-top-p ct2) ct1 nil)
      (bot)))
(defmethod subtract (client (ct1 cclass) (ct2 cfunction))
  (if (subfunction-cclass-p client ct1)
      (if (function-top-p ct2) (bot) nil)
      ct1))
(defmethod subtract (client (ct1 cfunction) (ct2 cclass))
  (if (subfunction-cclass-p client ct2)
      nil
      ct1))

;;; Some ctypes are never empty and also never top. Define this explicitly.
(defmacro defexistential (class)
  `(progn
     (defmethod subctypep (client (ct1 ,class) (ct2 disjunction))
       (declare (ignore client))
       (if (bot-p ct2)
           (values nil t)
           (values nil nil)))
     (defmethod subctypep (client (ct1 conjunction) (ct2 ,class))
       (declare (ignore client))
       (if (top-p ct1)
           (values nil t)
           (values nil nil)))))
(defexistential cclass)
(defexistential range)
(defexistential ccomplex)
(defexistential carray)
(defexistential charset)
(defexistential cfunction)

;;; See ccons-bottom-p above
(defmethod subctypep (client (ct1 ccons) (ct2 disjunction))
  (if (bot-p ct2)
      (ccons-bottom-p client ct1)
      (values nil nil)))
(defmethod subctypep (client (ct1 conjunction) (ct2 ccons))
  (if (top-p ct1)
      (values nil t)
      (values nil nil)))

;;; Some ctypes represent an infinite number of possible objects, so they are
;;; never subctypes of any member ctype.

(defmacro definfinite (class)
  `(defmethod subctypep (client (ct1 ,class) (ct2 cmember))
     (declare (ignore client))
     (values nil t)))

(definfinite range)
(definfinite ccomplex)
(definfinite carray)
(definfinite cfunction)
;;; User classes can always have more instances and so are always infinite.
;;; A few standard classes have a finite number of elements, such as
;;; NULL, various character types, and real ranges, but we canonicalize these
;;; as non-cclasses.
(definfinite cclass)

;; T is also infinite
(defmethod subctypep (client (ct1 conjunction) (ct2 cmember))
  (let ((cts (junction-ctypes ct1)))
    (if (null cts)
        (values nil t)
        (values nil nil))))

;; note that e.g. (cons (eql 1) (eql 1)) is still infinite, since you can keep
;; calling cons to get fresh conses of (1 . 1).
(defmethod subctypep (client (ct1 ccons) (ct2 cmember))
  (or/tri (ccons-bottom-p client ct1) (values nil t)))

;;; We normalize characters out of member types, so members never contain
;;; characters. charsets are not infinite, though.
(defmethod subctypep (client (ct1 charset) (ct2 cmember))
  (declare (ignore client))
  (values nil t))

;;; Resolve some (subtypep '(not X) '(member ...)) questions negatively.
(defmethod subctypep (client (ct1 negation) (ct2 cmember))
  (multiple-value-bind (cofinitep surety)
      (cofinitep client (negation-ctype ct1))
    (if (and surety (not cofinitep))
        (values nil t)
        (values nil nil))))

;;; These methods exist so that disjoin-cmember doesn't produce nested
;;; disjunctions, e.g. from (or boolean list) => (or (eql t) (or cons null))
(define-commutative-method disjoin/2 (client (cmember cmember) (disjunction disjunction))
  (let* ((scts (junction-ctypes disjunction))
         (non (loop for elem in (cmember-members cmember)
                    unless (loop for sct in scts
                                 thereis (ctypep client elem sct))
                      collect elem)))
    ;; We use disjoin instead of creating a disjunction in case one of our
    ;; disjunction ctypes is another cmember to be merged.
    ;; Inefficient? Probably.
    (apply #'disjoin client (apply #'cmember non) scts)))

;;; Deal with fpzeros and ranges.
(defmethod subctypep (client (ct1 fpzero) (ct2 range))
  (values (ctypep client (fpzero-zero ct1) ct2) t))
(defmethod subctypep (client (ct1 range) (ct2 fpzero))
  (declare (ignore client))
  (values nil t))

(define-commutative-method disjointp (client (ct1 fpzero) (ct2 range))
  (values (not (ctypep client (fpzero-zero ct1) ct2)) t))

(define-commutative-method conjoin/2 (client (ct1 fpzero) (ct2 range))
  (if (ctypep client (fpzero-zero ct1) ct2)
      ct1
      (bot)))

(define-commutative-method disjoin/2 (client (ct1 fpzero) (ct2 range))
  (if (ctypep client (fpzero-zero ct1) ct2)
      ct2
      nil))

(defmethod subtract (client (ct1 fpzero) (ct2 range))
  (if (ctypep client (fpzero-zero ct1) ct2)
      (bot)
      ct1))
(defmethod subtract (client (ct1 range) (ct2 fpzero))
  (let ((zero (fpzero-zero ct2)))
    (if (ctypep client zero ct1)
        ;; Here's the pain.
        (let ((k (range-kind ct1))
              (low (range-low ct1)) (lxp (range-low-exclusive-p ct1))
              (high (range-high ct1)) (hxp (range-high-exclusive-p ct1)))
          (cond ((and low (= zero low))
                 (disjunction (fpzero k (- zero))
                              (range k low t high hxp)))
                ((and high (= zero high))
                 (disjunction (fpzero k (- zero))
                              (range k low lxp high t)))
                (t
                 (disjunction (fpzero k (- zero))
                              (range k low lxp zero t)
                              (range k zero t high hxp)))))
        nil)))

;;; Try to answer some (subtypep t '(not ...)) and (subtypep '(not ...) nil)
;;; negatively.
;;; These should always terminate since we're removing a negation.
(defmethod subctypep (client (ct1 conjunction) (ct2 negation))
  (if (null (junction-ctypes ct1))
      (subctypep client (negation-ctype ct2) (bot))
      (values nil nil)))
(defmethod subctypep (client (ct1 negation) (ct2 disjunction))
  (if (null (junction-ctypes ct2))
      (subctypep client (top) (negation-ctype ct1))
      (values nil nil)))

;;; This is sort of the hardest method - including both
;;; (subtypep t ...) and (subtypep ... nil), which are hard problems in general.
;;; If you're wondering, the (disjunction conjunction) is easy - the methods on
;;; both (disjunction ctype) and (ctype conjunction) give comprehensive answers.
;;; FIXME: Hard it may be, but we can improve on this.
(defmethod subctypep (client (ct1 conjunction) (ct2 disjunction))
  (let ((cjs (junction-ctypes ct1)) (djs (junction-ctypes ct2)))
    (cond ((null cjs) ; (subtypep 't '(or ...))
           (case (length djs)
             ((0) (values nil t)) ; (subtypep 't 'nil)
             ;; degenerate; normalization ought to make this impossible
             ((1) (subctypep client ct1 (first djs)))
             ((2)
              ;; Special case: we can use conjointp, which will sometimes
              ;; give definitive negative answers: e.g.
              ;; (subtypep 't '(or cons integer)) is false.
              (conjointp client (first djs) (second djs)))
             (t (values nil nil))))
          ((null djs) ; (subtypep '(and ...) 'nil)
           (case (length cjs)
             ((1) (subctypep client (first cjs) ct2)) ; degenerate
             ((2) (disjointp client (first cjs) (second cjs)))
             (t (values nil nil))))
          (t ; (subtypep '(and ...) '(or ...))
           (values nil nil)))))
