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
  (ccons (disjoin (ccons-car ct1) (ccons-car ct2))
         (disjoin (ccons-cdr ct1) (ccons-cdr ct2))))

(defmethod subtract ((ct1 ccons) (ct2 ccons))
  (ccons (subtract (ccons-car ct1) (ccons-car ct2))
         (subtract (ccons-cdr ct1) (ccons-cdr ct2))))

(defmethod unparse ((ct ccons))
  (let ((car (ccons-car ct)) (cdr (ccons-cdr ct)))
    (if (top-p cdr)
        (if (top-p car)
            'cons
            `(cons ,(unparse car)))
        `(cons ,(unparse car) ,(unparse cdr)))))
