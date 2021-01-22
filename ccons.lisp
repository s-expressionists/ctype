(in-package #:ctype)

(defmethod ctypep ((object cons) (ct ccons))
  (and (ctypep (car object) (ccons-car ct))
       (ctypep (cdr object) (ccons-cdr ct))))
(defmethod ctypep ((object t) (ct ccons)) nil)

(defmethod subctypep ((ct1 ccons) (ct2 ccons))
  (multiple-value-bind (s1 surety1)
      (subctypep (ccons-car ct1) (ccons-car ct2))
    (cond (s1
           (multiple-value-bind (s2 surety2)
               (subctypep (ccons-cdr ct1) (ccons-cdr ct2))
             (cond (s2 (values t t))
                   (surety2 (values nil t))
                   (t (call-next-method)))))
          (surety1 (values nil t))
          (t (call-next-method)))))

(defmethod disjointp ((ct1 ccons) (ct2 ccons))
  (multiple-value-bind (s1 surety1)
      (disjointp (ccons-car ct1) (ccons-car ct2))
    (if s1
        (values t t)
        (multiple-value-bind (s2 surety2)
            (disjointp (ccons-cdr ct1) (ccons-cdr ct2))
          (if s2
              (values t t)
              (values nil (and surety1 surety2)))))))

(defmethod cofinitep ((ct1 ccons)) (values nil t))

(defmethod negate ((ctype ccons))
  ;; (not (cons a b))
  ;; = (or (not cons) (cons a (not b)) (cons (not a) b) (cons (not a) (not b)))
  ;; Or if A or B is the top type, some of these are eliminated.
  (let* ((car (ccons-car ctype)) (cdr (ccons-cdr ctype))
         (ncar (negate car)) (ncdr (negate cdr)))
    (if (bot-p ncar)
        (if (bot-p ncdr)
            ;; (not cons)
            (call-next-method)
            ;; car is t and cdr is not, so (or (not cons) (cons a (not b)))
            (disjunction
             (negation (ccons car car))
             (ccons car ncdr)))
        (if (bot-p ncdr)
            ;; (or (not cons) (cons (not a) b))
            (disjunction
             (negation (ccons cdr cdr))
             (ccons ncar cdr))
            ;; as in the main comment,
            ;; except we reduce the last two to (cons (not a) t)
            (let ((top (top)))
              (disjunction
               (negation (ccons top top))
               (ccons car ncdr)
               (ccons ncar top)))))))

(defmethod conjoin/2 ((ct1 ccons) (ct2 ccons))
  (ccons (conjoin (ccons-car ct1) (ccons-car ct2))
         (conjoin (ccons-cdr ct1) (ccons-cdr ct2))))

(defmethod disjoin/2 ((ct1 ccons) (ct2 ccons))
  ;; (or (cons a b) (cons b d)) is in general a strict subtype of
  ;; (cons (or a b) (cons b d)), which includes (cons a d) etc.
  ;; TODO: Enhance this. For now, we just do the special case of
  ;; (or (cons a b) (cons a d)) = (cons a (or b d)).
  (let ((car1 (ccons-car ct1)) (cdr1 (ccons-cdr ct1))
        (car2 (ccons-car ct2)) (cdr2 (ccons-cdr ct2)))
    (cond ((and (subctypep car1 car2) (subctypep car2 car1))
           (ccons car1 (disjoin cdr1 cdr2)))
          ((and (subctypep cdr1 cdr2) (subctypep cdr2 cdr1))
           (ccons (disjoin car1 car2) cdr1))
          (t (call-next-method)))))

(defmethod subtract ((ct1 ccons) (ct2 ccons))
  ;; similar concerns as in disjoin/2.
  (let ((car1 (ccons-car ct1)) (cdr1 (ccons-cdr ct1))
        (car2 (ccons-car ct1)) (cdr2 (ccons-cdr ct2)))
    (cond ((and (subctypep car1 car2) (subctypep car2 car1))
           (ccons car1 (conjoin cdr1 (negate cdr2))))
          ((and (subctypep cdr1 cdr2) (subctypep cdr2 cdr1))
           (ccons (conjoin car1 (negate car2)) cdr1))
          (t (call-next-method)))))

(defmethod unparse ((ct ccons))
  (let ((car (ccons-car ct)) (cdr (ccons-cdr ct)))
    (if (top-p cdr)
        (if (top-p car)
            'cons
            `(cons ,(unparse car)))
        `(cons ,(unparse car) ,(unparse cdr)))))
