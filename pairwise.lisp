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
(defexclusives cclass carray range ccomplex charset)
(defexclusives fpzero cmember ccomplex carray charset cfunction)

(macrolet ((consxclusive/1 (class)
             `(progn
                (defmethod subctypep (client (ct1 ccons) (ct2 ,class))
                  ;; ct1 and ct2 are basically exclusive, so if ct1 is
                  ;; definitely NOT bottom, they really are exclusive.
                  ;; That's why this is different from the general method above.
                  (emptyp client ct1))
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
  (consxclusive range ccomplex carray cclass charset cfunction fpzero))

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

;;; CONS is a subclass of SEQUENCE. Therefore, all CONS types are subtypes of
;;; SEQUENCE, regardless of whether they actually describe proper sequences,
;;; and even if they don't.
(defmethod subctypep (client (ct1 ccons) (ct2 csequence))
  (declare (ignore client))
  (values t t))
(defmethod subctypep (client (ct1 csequence) (ct2 ccons))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 ccons) (ct2 csequence))
  (declare (ignore client))
  (values nil t))
(define-commutative-method conjoin/2 (client (ct1 csequence) (ct2 ccons))
  (declare (ignore client))
  ct2)
(define-commutative-method disjoin/2 (client (ct1 csequence) (ct2 ccons))
  (declare (ignore client))
  ct1)
(defmethod subtract (client (ct1 ccons) (ct2 csequence))
  (declare (ignore client))
  (bot))
;;; NULL is (MEMBER NIL), and cmember methods should already handle things.
(defmethod subctypep (client (ct1 carray) (ct2 csequence))
  (declare (ignore client))
  (values (let ((dims (carray-dims ct1)))
            (and (listp dims) (= (length dims) 1)))
          t))
(defmethod subctypep (client (ct1 cclass) (ct2 carray))
  (declare (ignore client))
  (values nil t))
(define-commutative-method disjointp (client (ct1 carray) (ct2 csequence))
  (declare (ignore client))
  (values (not (let ((dims (carray-dims ct1)))
                 (or (eq dims '*) (= (length dims) 1))))
          t))
(define-commutative-method conjoin/2 (client (cs csequence) (carray carray))
  (declare (ignore client))
  (let ((dims (carray-dims carray)))
    (cond ((eq dims '*)
           (carray (carray-simplicity carray)
                   (carray-uaet carray) (carray-eaet carray)
                   '(*)))
          ((= (length dims) 1) carray)
          (t (bot)))))
(defmethod subtract (client (ct1 csequence) (ct2 carray))
  (declare (ignore client))
  (let ((dims (carray-dims ct2)))
    (if (or (eq dims '*) (= (length dims) 1))
        nil
        ct1)))
(defmethod subtract (client (ct1 carray) (ct2 csequence))
  (declare (ignore client))
  (let ((dims (carray-dims ct1)))
    (cond ((eq dims '*) nil)
          ((= (length dims) 1) (bot))
          (t ct1))))

(defun subfunction-cclass-p (client cclass)
  ;; FIXME: We skip the env here, is that okay?
  (subclassp client (cclass-class cclass) (find-class client 'function t)))
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

;;; We normalize characters out of member types, so members never contain
;;; characters. charsets are not infinite, though.
;;; Reals are also normalized out.
(defexclusives cmember charset range)

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

;;; dumb KLUDGE: the standard requires that (subtypep 'keyword nil)
;;; and (subtypep 'compiled-function nil) give valid answers (i.e., false)
;;; but there's no natural way to represent these types.
;;; We use SATISFIES, which is hard to do anything with.
;;; Here we special case some SATISFIES disjointp queries so that these two
;;; types are known to be above NIL. I think this is the last bad way to do it.
;;; Note that we do _not_ define subctypep for the admittedly dumb reason that
;;; (and function (satisfies ...)) is easier to work with in some ways than
;;; (satisfies compiled-function-p) would be, and ditto keywordp.
(define-commutative-method disjointp (client (class1 csatisfies) (class2 cclass))
  (declare (ignore client))
  (if (and (eq (csatisfies-fname class1) 'keywordp)
           (eq (class-name (cclass-class class2)) 'symbol))
      (values nil t)
      (values nil nil)))
(define-commutative-method disjointp
    (client (class1 csatisfies) (class2 cfunction))
  (declare (ignore client))
  (if (and (eq (csatisfies-fname class1) 'compiled-function-p)
           (function-top-p class2))
      (values nil t)
      (values nil nil)))
