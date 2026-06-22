(in-package #:ctype)

(defmethod ctypep (client (object cons) (ct ccons))
  (and (ctypep client (car object) (ccons-car ct))
       (ctypep client (cdr object) (ccons-cdr ct))))
(defmethod ctypep (client (object t) (ct ccons))
  (declare (ignore client))
  nil)

(defmethod subctypep (client (ct1 ccons) (ct2 ccons))
  (and/tri (subctypep client (ccons-car ct1) (ccons-car ct2))
           (subctypep client (ccons-cdr ct1) (ccons-cdr ct2))))

(defmethod ctype= (client (ct1 ccons) (ct2 ccons))
  (and/tri (ctype= client (ccons-car ct1) (ccons-car ct2))
           (ctype= client (ccons-cdr ct1) (ccons-cdr ct2))))

(defmethod disjointp (client (ct1 ccons) (ct2 ccons))
  (or/tri (disjointp client (ccons-car ct1) (ccons-car ct2))
          (disjointp client (ccons-cdr ct1) (ccons-cdr ct2))))
(defmethod conjointp (client (ct1 ccons) (ct2 ccons))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct1 ccons))
  (declare (ignore client))
  (values nil t))

(defmethod negate (client (ctype ccons))
  ;; (not (cons a b))
  ;; = (or (not cons) (cons a (not b)) (cons (not a) b) (cons (not a) (not b)))
  ;; Or if A or B is the top type, some of these are eliminated.
  (let* ((car (ccons-car ctype)) (cdr (ccons-cdr ctype))
         (ncar (negate client car)) (ncdr (negate client cdr)))
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

(defmethod conjoin/2 (client (ct1 ccons) (ct2 ccons))
  (ccons (conjoin client (ccons-car ct1) (ccons-car ct2))
         (conjoin client (ccons-cdr ct1) (ccons-cdr ct2))))

(defmethod disjoin/2 (client (ct1 ccons) (ct2 ccons))
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
         (car2 (ccons-car ct2)) (cdr2 (ccons-cdr ct2)))
    (cond ((ctype= client car1 car2) (ccons car1 (disjoin client cdr1 cdr2)))
          ((ctype= client cdr1 cdr2) (ccons (disjoin client car1 car2) cdr1))
          ;; In the following, the subtractions should really never be bottom,
          ;; because one is a strict subtype of the other. If one is bottom it
          ;; would indicate that the simplifier (like conjoin/2) is smarter
          ;; than subctypep, which is unfortunate.
          ((subctypep client car1 car2)
           (let ((car-2-1 (conjoin client car2 (negate client car1)))
                 (reg (ccons car1 (disjoin client cdr1 cdr2))))
             (if (bot-p car-2-1)
                 reg
                 (disjunction reg (ccons car-2-1 cdr2)))))
          ((subctypep client car2 car1)
           (let ((car-1-2 (conjoin client car1 (negate client car2)))
                 (reg (ccons car2 (disjoin client cdr1 cdr2))))
             (if (bot-p car-1-2)
                 reg
                 (disjunction reg (ccons car-1-2 cdr1)))))
          ;; Give up unless we can prove nondisjointness.
          ((multiple-value-bind (val1 surety1) (disjointp client car1 car2)
             (or val1
                 (not surety1)
                 (multiple-value-bind (val2 surety2) (disjointp client cdr1 cdr2)
                   (or val2 (not surety2)))))
           nil)
          ((subctypep client cdr1 cdr2)
           (let ((cdr-2-1 (conjoin client cdr2 (negate client cdr1)))
                 (reg (ccons (disjoin client car1 car2) cdr1)))
             (if (bot-p cdr-2-1)
                 reg
                 (disjunction reg (ccons car2 cdr-2-1)))))
          ((subctypep client cdr2 cdr1)
           (let ((cdr-1-2 (conjoin client cdr1 (negate client cdr2)))
                 (reg (ccons (disjoin client car1 car2) cdr2)))
             (if (bot-p cdr-1-2)
                 reg
                 (disjunction reg (ccons car1 cdr-1-2)))))
          (t nil))))

(defmethod subtract (client (ct1 ccons) (ct2 ccons))
  ;; as in the negate method, (not (cons a b)) =
  ;; (or (not cons) (cons a (not b)) (cons (not a) b) (cons (not a) (not b)))
  ;; We're conjoining this with (cons c d).
  ;; (and (cons c d) (not cons)) is obviously nil.
  ;; (and (cons c d) (cons a (not b))) = (cons (and c a) (and d (not b)))
  ;; (and (cons c d) (cons (not a) b)) = (cons (and c (not a)) (and d b))
  ;; (and (cons c d) (cons (not a) (not b)))
  ;;  = (cons (and c (not a)) (and d (not b)))
  ;; These types are obviously pairwise disjoint so we can just use disjunction
  ;; directly and save a bit of time. And there are special cases:
  ;; If (and c a) = 0, (and c (not a)) = c, so we have
  ;; (or (cons c (and d b)) (cons c (and d (not b)))) = (cons c d)
  ;; If c <: a, (and c a) = c and (and c (not a)) = 0, so we have
  ;; (cons c (and d (not b))); if also d <: b this is 0.
  (let ((car1 (ccons-car ct1)) (cdr1 (ccons-cdr ct1))
        (car2 (ccons-car ct1)) (cdr2 (ccons-cdr ct2)))
    (cond ((disjointp client car1 car2) ct1)
          ((disjointp client cdr1 cdr2) ct1)
          ((subctypep client car1 car2)
           (if (subctypep client cdr1 cdr2)
               (bot)
               (ccons car1 (conjoin client cdr1 (negate client cdr2)))))
          ((subctypep cdr1 cdr2)
           (ccons (conjoin client car1 (negate client car2)) cdr1))
          (t (let ((car1-2 (conjoin client car1 (negate client car2)))
                   (cdr1-2 (conjoin client cdr1 (negate client cdr2))))
               (disjunction (ccons (conjoin client car1 car2) cdr1-2)
                            (ccons car1-2 (conjoin client cdr1 cdr2))
                            (ccons car1-2 cdr1-2)))))))

(defmethod unparse ((ct ccons))
  (let ((car (ccons-car ct)) (cdr (ccons-cdr ct)))
    (if (top-p cdr)
        (if (top-p car)
            'cons
            `(cons ,(unparse car)))
        `(cons ,(unparse car) ,(unparse cdr)))))
