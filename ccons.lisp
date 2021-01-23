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
  ;; (or (cons a b) (cons c d)) is in general a strict subtype of
  ;; (cons (or a b) (or c d)), which includes (cons a d) etc.
  ;; It can be written out more explicitly as
  ;; (or (cons (and a (not c)) (and b (not d)))
  ;;     (cons (and a (not c)) (and b d))
  ;;     (cons (and a c)       (and b (not d)))
  ;;     (cons (and a c)       (and b d))
  ;;     (cons (and a c)       (and (not b) d))
  ;;     (cons (and (not a) c) (and (not b) d))
  ;;     (cons (and (not a) c) (and b d)))
  ;; using a = (or (and a c) (and a (not c))), etc.,
  ;; and in which all the cons types are pairwise disjoint.
  ;; But that's pretty ugly in general.
  ;; If a <: c, (and a (not c)) = nil, and (and a c) = a, so we have
  ;; (or (cons a               (and b (not d)))
  ;;     (cons a               (and b d))
  ;;     (cons a               (and (not b) d))
  ;;     (cons (and (not a) c) (and (not b) d))
  ;;     (cons (and (not a) c) (and b d)))
  ;; which could be further reduced to
  ;; (or (cons a               (or b d))
  ;;     (cons (and (not a) c) d))
  ;; if c <: a as well (i.e. a = c), (and (not a) c) = nil, so it's just
  ;; (cons a (or b d)).
  ;; You can swap a and c, or do this with b and d, obviously.
  ;; Alternately, if a and c are disjoint, (and a c) = nil, (and a (not c)) = a,
  ;; and (and (not a) c) = c, so we have
  ;; (or (cons a (and b (not d)))
  ;;     (cons a (and b d))
  ;;     (cons c (and (not b) d))
  ;;     (cons c (and b d)))
  ;; which can be further reduced to (or (cons a b) (cons c d)) again
  ;; but with the understanding that they are disjoint. Not too helpful?
  ;; SO, in order of preference, if there's a type equality we can reduce
  ;; to a cons type; or if there's a subtype we can reduce to two modified
  ;; cons types; or we just make a general union.
  ;; We can also check if a <: (not c) and such, but I think at that point we
  ;; may as well actually computer intersections anyway.
  ;; We have to be careful to avoid "flipping". With the above rules,
  ;; (or (cons t (not symbol)) (cons (not symbol) symbol))
  ;; is "simplified" to (or (cons (not symbol) t) (cons symbol (not symbol)))
  ;; which in turn "simplifies" back to where it started! Very bad.
  ;; So we only do it on the cdrs (arbitrarily chosen) if we know that the
  ;; cons types are not disjoint.
  (let* ((car1 (ccons-car ct1)) (cdr1 (ccons-cdr ct1))
         (car2 (ccons-car ct2)) (cdr2 (ccons-cdr ct2))
         (st-car-12 (subctypep car1 car2)) (st-car-21 (subctypep car2 car1))
         (st-cdr-12 (subctypep cdr1 cdr2)) (st-cdr-21 (subctypep cdr2 cdr1)))
    (cond ((and st-car-12 st-car-21) ; cars are equal
           (ccons car1 (disjoin cdr1 cdr2)))
          ((and st-cdr-12 st-cdr-21) ; cdrs are equal
           (ccons (disjoin car1 car2) cdr1))
          ;; In the following, the subtractions should really never be bottom,
          ;; because one is a strict subtype of the other. If one is bottom it
          ;; would indicate that the simplifier (like conjoin/2) is smarter
          ;; than subctypep, which is unfortunate.
          (st-car-12
           (let ((car-2-1 (conjoin car2 (negate car1)))
                 (reg (ccons car1 (disjoin cdr1 cdr2))))
             (if (bot-p car-2-1)
                 reg
                 (disjunction reg (ccons car-2-1 cdr2)))))
          (st-car-21
           (let ((car-1-2 (conjoin car1 (negate car2)))
                 (reg (ccons car2 (disjoin cdr1 cdr2))))
             (if (bot-p car-1-2)
                 reg
                 (disjunction reg (ccons car-1-2 cdr1)))))
          ;; Give up unless we can prove nondisjointness.
          ((multiple-value-bind (val1 surety1) (disjointp car1 car2)
             (or val1
                 (not surety1)
                 (multiple-value-bind (val2 surety2) (disjointp cdr1 cdr2)
                   (or val2 (not surety2)))))
           (call-next-method))
          (st-cdr-12
           (let ((cdr-2-1 (conjoin cdr2 (negate cdr1)))
                 (reg (ccons (disjoin car1 car2) cdr1)))
             (if (bot-p cdr-2-1)
                 reg
                 (disjunction reg (ccons car2 cdr-2-1)))))
          (st-cdr-21
           (let ((cdr-1-2 (conjoin cdr1 (negate cdr2)))
                 (reg (ccons (disjoin car1 car2) cdr2)))
             (if (bot-p cdr-1-2)
                 reg
                 (disjunction reg (ccons car1 cdr-1-2)))))
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
