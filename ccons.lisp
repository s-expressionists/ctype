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
             (negation (cons cdr cdr))
             (ccons ncar cdr))
            ;; as in the main comment,
            ;; except we reduce the last two to (cons (not a) t)
            (let ((top (top)))
              (disjunction
               (negation (ccons top top))
               (ccons car ncdr)
               (ccons ncar top)))))))

(defmethod conjoin/2 ((ct1 ccons) (ct2 ccons))
  (let ((car (conjoin (ccons-car ct1) (ccons-car ct2)))
        (cdr (conjoin (ccons-cdr ct1) (ccons-cdr ct2))))
    (if (or (bot-p car) (bot-p cdr))
        (bot)
        (ccons car cdr))))

(defmethod disjoin/2 ((ct1 ccons) (ct2 ccons))
  (let ((car (disjoin (ccons-car ct1) (ccons-car ct2)))
        (cdr (disjoin (ccons-cdr ct1) (ccons-cdr ct2))))
    (ccons car cdr)))

(defmethod unparse ((ct ccons))
  (let ((car (ccons-car ct)) (cdr (ccons-cdr ct)))
    (if (top-p cdr)
        (if (top-p car)
            'cons
            `(cons ,(unparse car)))
        `(cons ,(unparse car) ,(unparse cdr)))))
