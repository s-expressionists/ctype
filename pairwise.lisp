(in-package #:ctype)

;;;; Pair methods
;;;; That is, methods on two specific ctype classes.

;;; cclass ctypes are excluded from several other ctypes (when things are
;;; normalized correctly), so we can mark their conjunctions as empty, etc.

(defmacro defexclusive/2 (class1 class2)
  `(progn
     (defmethod subctypep ((ct1 ,class1) (ct2 ,class2)) (values nil t))
     (defmethod subctypep ((ct1 ,class2) (ct2 ,class1)) (values nil t))
     (defmethod disjointp ((ct1 ,class1) (ct2 ,class2)) (values t t))
     (defmethod disjointp ((ct1 ,class2) (ct2 ,class1)) (values t t))
     (defmethod conjoin/2 ((ct1 ,class1) (ct2 ,class2)) (bot))
     (defmethod conjoin/2 ((ct1 ,class2) (ct2 ,class1)) (bot))
     (defmethod subtract ((ct1 ,class1) (ct2 ,class2)) ct1)
     (defmethod subtract ((ct2 ,class2) (ct1 ,class1)) ct2)))

(defmacro defexclusive (&rest classes)
  `(progn
     ,@(loop for (class1 . rest) on classes
             nconc (loop for class2 in rest
                         collect `(defexclusive/2 ,class1 ,class2)))))

(defexclusive range ccomplex carray charset cfunction)
(defexclusive/2 cclass range)
(defexclusive/2 cclass ccomplex)
(defexclusive/2 cclass charset)
(defexclusive/2 fpzero cmember)
(defexclusive/2 fpzero ccomplex)
(defexclusive/2 fpzero carray)
(defexclusive/2 fpzero charset)
(defexclusive/2 fpzero cfunction)

;;; cons types are unfortunately ambiguous: (cons (satisfies foo)) MIGHT be
;;; bottom "in disguise", and might not be.
;;; This is basically (subctypep ccons (bot)), but we use this in subctypep
;;; methods themselves, so we have to reduce it to mimic the usual subctypep
;;; method in ccons.lisp.
;;; FIXME: Is the lack of call-next-method a problem? I don't think it is if
;;; we only use this in subctypep/disjointp methods?
(defun ccons-bottom-p (ccons)
  (or/tri (subctypep (ccons-car ccons) (bot))
          (subctypep (ccons-cdr ccons) (bot))))

(macrolet ((consxclusive/1 (class)
             `(progn
                (defmethod subctypep ((ct1 ccons) (ct2 ,class))
                  (surely (ccons-bottom-p ct1) (call-next-method)))
                (defmethod subctypep ((ct1 ,class) (ct2 ccons)) (values nil t))
                (defmethod disjointp ((ct1 ccons) (ct2 ,class)) (values t t))
                (defmethod disjointp ((ct1 ,class) (ct2 ccons)) (values t t))
                (defmethod conjoin/2 ((ct1 ccons) (ct2 ,class)) (bot))
                (defmethod conjoin/2 ((ct1 ,class) (ct2 ccons)) (bot))
                (defmethod subtract ((ct1 ccons) (ct2 ,class)) ct1)
                (defmethod subtract ((ct2 ,class) (ct1 ccons)) ct2)))
           (consxclusive (&rest classes)
             `(progn ,@(loop for class in classes
                             collect `(consxclusive/1 ,class)))))
  (consxclusive range ccomplex carray charset cfunction fpzero))

(macrolet ((defnonconjoint/2 (c1 c2)
             `(progn
                (defmethod conjointp ((ct1 ,c1) (ct2 ,c2)) (values nil t))
                (defmethod conjointp ((ct1 ,c2) (ct2 ,c1)) (values nil t))))
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
(defmethod subctypep ((ct1 ccons) (ct2 cclass))
  (or/tri (ccons-bottom-p ct1)
          (values (sequence-cclass-p ct2) t)))
(defmethod subctypep ((ct1 cclass) (ct2 ccons)) (values nil t))
(defmethod disjointp ((ct1 ccons) (ct2 cclass))
  (or/tri (ccons-bottom-p ct1)
          (values (not (sequence-cclass-p ct2)) t)))
(defmethod disjointp ((ct1 cclass) (ct2 ccons))
  (or/tri (ccons-bottom-p ct2)
          (values (not (sequence-cclass-p ct1)) t)))
(defmethod conjoin/2 ((ct1 cclass) (ct2 ccons))
  (if (sequence-cclass-p ct1) ct2 (bot)))
(defmethod conjoin/2 ((ct1 ccons) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct1 (bot)))
(defmethod subtract ((ct1 ccons) (ct2 cclass))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract ((ct1 cclass) (ct2 ccons))
  (if (sequence-cclass-p ct1) (call-next-method) (bot)))
;;; NULL is (MEMBER NIL), and cmember methods should already handle things.
(defmethod subctypep ((ct1 carray) (ct2 cclass))
  (values (and (sequence-cclass-p ct2)
               (let ((dims (carray-dims ct1)))
                 (and (listp dims) (= (length dims) 1))))
          t))
(defmethod subctypep ((ct1 cclass) (ct2 carray)) (values nil t))
(defmethod disjointp ((ct1 carray) (ct2 cclass))
  (values (not (and (sequence-cclass-p ct2)
                    (let ((dims (carray-dims ct1)))
                      (or (eq dims '*) (= (length dims) 1)))))
          t))
(defmethod disjointp ((ct1 cclass) (ct2 carray))
  (values (not (and (sequence-cclass-p ct1)
                    (let ((dims (carray-dims ct2)))
                      (or (eq dims '*) (= (length dims) 1)))))
          t))
(defun conjoin-cclass-carray (cclass carray)
  (if (sequence-cclass-p cclass)
      (let ((dims (carray-dims carray)))
        (cond ((eq dims '*)
               (carray (carray-simplicity carray) (carray-uaet carray) '(*)))
              ((= (length dims) 1) carray)
              (t (bot))))
      (bot)))
(defmethod conjoin/2 ((ct1 cclass) (ct2 carray))
  (conjoin-cclass-carray ct1 ct2))
(defmethod conjoin/2 ((ct1 carray) (ct2 cclass))
  (conjoin-cclass-carray ct2 ct1))
(defmethod subtract ((ct1 cclass) (ct2 carray))
  (if (sequence-cclass-p ct1)
      (let ((dims (carray-dims ct2)))
        (if (or (eq dims '*) (= (length dims) 1))
            (call-next-method)
            ct1))
      ct1))
(defmethod subtract ((ct1 carray) (ct2 cclass))
  (if (sequence-cclass-p ct2)
      (let ((dims (carray-dims ct1)))
        (cond ((eq dims '*) (call-next-method))
              ((= (length dims) 1) (bot))
              (t ct1)))
      ct1))

(defun subfunction-cclass-p (cclass)
  ;; FIXME: We skip the env here, is that okay?
  (subclassp (cclass-class cclass) (find-class 'function t)))
(defmethod subctypep ((ct1 cfunction) (ct2 cclass))
  ;; FUNCTION itself is never a cclass, so
  (values nil t))
(defmethod subctypep ((ct1 cclass) (ct2 cfunction))
  (if (subfunction-cclass-p ct1)
      (if (top-function-p ct2) (values t t) (values nil nil))
      (values nil t)))
(defmethod conjoin/2 ((ct1 cclass) (ct2 cfunction))
  (if (subfunction-cclass-p ct1)
      (if (top-function-p ct2) ct1 (call-next-method))
      (bot)))
(defmethod conjoin/2 ((ct1 cfunction) (ct2 cclass))
  (if (subfunction-cclass-p ct2)
      (if (top-function-p ct1) ct2 (call-next-method))
      (bot)))
(defmethod subtract ((ct1 cclass) (ct2 cfunction))
  (if (subfunction-cclass-p ct1)
      (if (top-function-p ct2) (bot) (call-next-method))
      ct1))
(defmethod subtract ((ct1 cfunction) (ct2 cclass))
  (if (subfunction-cclass-p ct2)
      (call-next-method)
      ct1))

;;; Some ctypes are never empty and also never top. Define this explicitly.
(defmacro defexistential (class)
  `(progn
     (defmethod subctypep ((ct1 ,class) (ct2 disjunction))
       (if (bot-p ct2)
           (values nil t)
           (call-next-method)))
     (defmethod subctypep ((ct1 conjunction) (ct2 ,class))
       (if (top-p ct1)
           (values nil t)
           (call-next-method)))))
(defexistential cclass)
(defexistential range)
(defexistential ccomplex)
(defexistential carray)
(defexistential charset)
(defexistential cfunction)

;;; See ccons-bottom-p above
(defmethod subctypep ((ct1 ccons) (ct2 disjunction))
  (if (bot-p ct2)
      (ccons-bottom-p ct1)
      (call-next-method)))
(defmethod subctypep ((ct1 conjunction) (ct2 ccons))
  (if (top-p ct1)
      (values nil t)
      (call-next-method)))

;;; Some ctypes represent an infinite number of possible objects, so they are
;;; never subctypes of any member ctype.

(defmacro definfinite (class)
  `(defmethod subctypep ((ct1 ,class) (ct2 cmember)) (values nil t)))

(definfinite ccons)
(definfinite range)
(definfinite ccomplex)
(definfinite carray)
(definfinite cfunction)

(defmethod subctypep ((ct1 ccons) (ct2 cmember))
  (or/tri (ccons-bottom-p ct1) (values nil t)))

;;; We normalize characters out of member types, so members never contain
;;; characters. charsets are not infinite, though.
(defmethod subctypep ((ct1 charset) (ct2 cmember)) (values nil t))

;;; Resolve some (subtypep '(not X) '(member ...)) questions negatively.
(defmethod subctypep ((ct1 negation) (ct2 cmember))
  (multiple-value-bind (cofinitep surety)
      (cofinitep (negation-ctype ct1))
    (if (and surety (not cofinitep))
        (values nil t)
        (call-next-method))))

;;; These methods exist so that disjoin-cmember doesn't produce nested
;;; disjunctions, e.g. from (or boolean list) => (or (eql t) (or cons null))
(defun disjoin-cmember-disjunction (cmember disjunction)
  (let* ((scts (junction-ctypes disjunction))
         (non (loop for elem in (cmember-members cmember)
                    unless (loop for sct in scts
                                   thereis (ctypep elem sct))
                      collect elem)))
    ;; We use disjoin instead of creating a disjunction in case one of our
    ;; disjunction ctypes is another cmember to be merged.
    ;; Inefficient? Probably.
    (apply #'disjoin (apply #'cmember non) scts)))
(defmethod disjoin/2 ((ct1 cmember) (ct2 disjunction))
  (disjoin-cmember-disjunction ct1 ct2))
(defmethod disjoin/2 ((ct1 disjunction) (ct2 cmember))
  (disjoin-cmember-disjunction ct2 ct1))

;;; Deal with fpzeros and ranges.
(defmethod subctypep ((ct1 fpzero) (ct2 range))
  (values (ctypep (fpzero-zero ct1) ct2) t))
(defmethod subctypep ((ct1 range) (ct2 fpzero)) (values nil t))

(defmethod disjointp ((ct1 fpzero) (ct2 range))
  (values (not (ctypep (fpzero-zero ct1) ct2)) t))
(defmethod disjointp ((ct1 range) (ct2 fpzero))
  (values (not (ctypep (fpzero-zero ct2) ct1)) t))

(defmethod conjoin/2 ((ct1 fpzero) (ct2 range))
  (if (ctypep (fpzero-zero ct1) ct2)
      ct1
      (bot)))
(defmethod conjoin/2 ((ct1 range) (ct2 fpzero))
  (if (ctypep (fpzero-zero ct2) ct1)
      ct2
      (bot)))

(defmethod disjoin/2 ((ct1 fpzero) (ct2 range))
  (if (ctypep (fpzero-zero ct1) ct2)
      ct2
      (call-next-method)))
(defmethod disjoin/2 ((ct1 range) (ct2 fpzero))
  (if (ctypep (fpzero-zero ct2) ct1)
      ct1
      (call-next-method)))

(defmethod subtract ((ct1 fpzero) (ct2 range))
  (if (ctypep (fpzero-zero ct1) ct2)
      (bot)
      ct1))
(defmethod subtract ((ct1 range) (ct2 fpzero))
  (let ((zero (fpzero-zero ct2)))
    (if (ctypep zero ct1)
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
                              (range k zero t high hxp))))))))

;;; This is sort of the hardest method - including both
;;; (subtypep t ...) and (subtypep ... nil), which are hard problems in general.
;;; If you're wondering, the (disjunction conjunction) is easy - the methods on
;;; both (disjunction ctype) and (ctype conjunction) give comprehensive answers.
;;; FIXME: Hard it may be, but we can improve on this.
(defmethod subctypep ((ct1 conjunction) (ct2 disjunction))
  (let ((cjs (junction-ctypes ct1)) (djs (junction-ctypes ct2)))
    (cond ((null cjs) ; (subtypep 't '(or ...))
           (case (length djs)
             ((0) (values nil t)) ; (subtypep 't 'nil)
             ;; degenerate; normalization ought to make this impossible
             ((1) (subctypep ct1 (first djs)))
             ((2)
              ;; Special case: we can use conjointp, which will sometimes
              ;; give definitive negative answers: e.g.
              ;; (subtypep 't '(or cons integer)) is false.
              (conjointp (first djs) (second djs)))
             (t (call-next-method))))
          ((null djs) ; (subtypep '(and ...) 'nil)
           (case (length cjs)
             ((1) (subctypep (first cjs) ct2)) ; degenerate
             ((2) (disjointp (first cjs) (second cjs)))
             (t (call-next-method))))
          (t ; (subtypep '(and ...) '(or ...))
           (call-next-method)))))
