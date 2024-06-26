(in-package #:ctype)

(defmethod ctypep (client object (ct disjunction))
  (loop for sct in (junction-ctypes ct)
          thereis (ctypep client object sct)))

(defmethod subctypep (client (ct1 disjunction) (ct2 ctype))
  ;; if a ~<: z then a v b ~<: z as a <: a v b.
  ;; if a <: z and b <: z, a v b <: z, as can be seen from a <: z <=> a ^ z = a:
  ;; a v b <: z <=> (a v b) ^ z = a v b <=> (a ^ z) v (b ^ z) = a v b
  ;; this also covers the case of ct1 being bot.
  (every/tri (lambda (sct) (subctypep client sct ct2)) (junction-ctypes ct1)))
(defmethod subctypep (client (ct1 ctype) (ct2 disjunction))
  #+(or)
  (or/tri
   (some/tri (lambda (sct) (subctypep client ct1 sct)) (junction-ctypes ct2))
   (call-next-method))
  ;; a <: (z v y) <=> a ^ (z v y) = a <=> (a ^ z) v (a ^ y) = a
  ;; Obviously if a <: z this reduces to a v (a ^ y) = a which is true. Ditto y.
  ;; Getting a definite negative result is a little more challenging.
  ;; First, we can see that if a ^ z = 0, (a ^ z) v (a ^ y) = a <=> a ^ y = a
  ;; <=> a <: y. With more elements, say a <: (z v y v x), the reduction is
  ;; to (a ^ y) v (a ^ x) = a <=> a <: (y v x), i.e. z becomes irrelevant.
  ;; So: if a <: z, a <: (z v y). If a is disjoint with all but one, the
  ;; result for that last one controls.
  ;; If a ^ z = 0 and a ^ y = 0, the question becomes a = 0, which we can answer
  ;; false if we also know that a ~<: z or a ~<: y.
  ;; The ambiguous case is when we don't know that a is a subtype of at least
  ;; one and don't know that a is disjoint with all or all but one.
  ;; These are sufficient but not necessary conditions.
  ;; This all covers questions like
  ;; (subtypep '(integer 10) '(rational 11))
  ;; (where the rational is broken up into an integer and a ratio range).
  (loop with surety = t
        with not-subtype = 0
        with not-subtype-and-not-disjoint = 0
        for sct in (junction-ctypes ct2)
        do (multiple-value-bind (val subsurety) (subctypep client ct1 sct)
             (cond ((not subsurety) (setf surety nil))
                   (val (return (values t t)))
                   (surety ; if we're unsure, this fancier stuff is out
                    (incf not-subtype)
                    (multiple-value-bind (val subsurety)
                        (disjointp client ct1 sct)
                      (cond ((not subsurety) (setf surety nil))
                            ((not val) (incf not-subtype-and-not-disjoint)))))))
        finally (return (if (and surety
                                 (or (= not-subtype-and-not-disjoint 1)
                                     (and (> not-subtype 0)
                                          (= not-subtype-and-not-disjoint 0))))
                            (values nil t)
                            (values nil nil)))))

(define-commutative-method disjointp (client (ct1 disjunction) (ct2 ctype))
  ;; (a v b) ^ z = 0 <=> (a ^ z) v (b ^ z) = 0
  (every/tri (lambda (sct) (disjointp client sct ct2)) (junction-ctypes ct1)))
(define-commutative-method conjointp (client (ct1 disjunction) (ct2 ctype))
  (if (some/tri (lambda (sct) (conjointp client sct ct2)) (junction-ctypes ct1))
      (values t t)
      (values nil nil)))

(defmethod negate (client (ctype disjunction))
  (apply #'conjoin
         client
         (mapcar (lambda (ct) (negate client ct)) (junction-ctypes ctype))))

(defmethod disjoin/2 (client (ct1 disjunction) (ct2 disjunction))
  (apply #'disjoin client (append (junction-ctypes ct1)
                                  (junction-ctypes ct2))))
(define-commutative-method disjoin/2 (client (ct1 disjunction) (ct2 ctype))
  (apply #'disjoin client ct2 (junction-ctypes ct1)))

(define-commutative-method conjoin/2
    (client (disjunction disjunction) (ctype ctype))
  (apply #'disjoin client
         (loop for sct in (junction-ctypes disjunction)
               collect (conjoin client sct ctype))))

(defmethod unparse ((ct disjunction))
  (let ((ups (mapcar #'unparse (junction-ctypes ct))))
    ;; special cases
    (when (null ups) (return-from unparse 'nil))
    ;; list
    (when (and (member 'null ups) (member 'cons ups))
      (setf ups (delete 'null ups)
            ups (delete 'cons ups))
      (push 'list ups))
    ;; rational (i.e. (rational * *))
    (when (and (member 'integer ups) (member 'ratio ups))
      (setf ups (delete 'integer ups)
            ups (delete 'ratio ups))
      (push 'rational ups))
    ;; real (again, unbounded only)
    (when (and (member 'float ups) (member 'rational ups))
      (setf ups (delete 'float ups)
            ups (delete 'rational ups))
      (push 'real ups))
    ;; bounded rational
    (let ((integer-ranges
            (loop for up in ups
                  when (and (listp up) (eq (first up) 'integer))
                    collect up))
          (ratio-ranges
            ;; ratios are unparsed as (and (not integer) (rational ...))
            (loop for up in ups
                  when (and (listp up) (= (length up) 3)
                            (eq (first up) 'and)
                            (equal (second up) '(not integer))
                            (listp (third up))
                            (eq (car (third up)) 'rational))
                    collect up)))
      (loop for ratio-range in ratio-ranges
            do (destructuring-bind (_ &optional (low '*) (high '*))
                   (third ratio-range)
                 (declare (ignore _))
                 ;; FIXME: Collapse (or (ratio (0)) (integer 0))
                 (let* ((ilow (cond ((eq low '*) low)
                                    ((listp low)
                                     (let ((l (car low)))
                                       (if (integerp l)
                                           (1+ l)
                                           (ceiling l))))
                                    (t (ceiling low))))
                        (ihigh (cond ((eq high '*) high)
                                     ((listp high)
                                      (let ((h (car high)))
                                        (if (integerp h)
                                            (1- h)
                                            (floor h))))
                                     (t (floor high))))
                        (tail (if (eq ihigh '*)
                                  (list ilow)
                                  (list ilow ihigh)))
                        (integer-range (find tail integer-ranges
                                             :key #'cdr :test #'equal)))
                   (when integer-range
                     (setf ups (delete ratio-range ups)
                           ups (delete integer-range ups))
                     (push (if (eq ihigh '*)
                               `(rational ,low)
                               `(rational ,low ,high))
                           ups))))))
    ;; finally,
    (if (= (length ups) 1)
        (first ups)
        `(or ,@ups))))
