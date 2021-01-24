(in-package #:ctype)

(defmethod ctypep (object (ct disjunction))
  (loop for sct in (junction-ctypes ct)
          thereis (ctypep object sct)))

(defmethod subctypep ((ct1 disjunction) (ct2 ctype))
  ;; if a ~<: z then a v b ~<: z as a <: a v b.
  ;; if a <: z and b <: z, a v b <: z, as can be seen from a <: z <=> a ^ z = a:
  ;; a v b <: z <=> (a v b) ^ z = a v b <=> (a ^ z) v (b ^ z) = a v b
  ;; this also covers the case of ct1 being bot.
  (surely
   (every/tri (lambda (sct) (subctypep sct ct2)) (junction-ctypes ct1))
   (call-next-method)))
(defmethod subctypep ((ct1 ctype) (ct2 disjunction))
  #+(or)
  (or/tri
   (some/tri (lambda (sct) (subctypep ct1 sct)) (junction-ctypes ct2))
   (call-next-method))
  ;; a <: (z v y) <=> a ^ (z v y) = a <=> (a ^ z) v (a ^ y) = a
  ;; Obviously if a <: z this reduces to a v (a ^ y) = a which is true. Ditto y.
  ;; Getting a definite negative result is a little more challenging.
  ;; One special case is that if a ^ z = 0, the question is a ^ y = a.
  ;; That is to say, if a is (definitely) not a subtype of y, and a is
  ;; (definitely) disjoint from z, a is not a subtype of z v y.
  ;; For a disjunction with any number of terms like this, a better phrasing
  ;; might be: if a is definitely disjoint with all the junction types, except
  ;; for one which it is definitely not a subtypep of, a is not a subtype of the
  ;; disjunction.
  ;; Note that if a is disjoint from ALL of them, this doesn't work.
  ;; Practically speaking this would only come up if we could prove disjointness
  ;; for all of them but not understand subtypes, which is unlikely, but we
  ;; check for it anyway because all this math is hard enough without making
  ;; assumptions about how correct the rest of the system is.
  ;; Anyway, this all covers questions like
  ;; (subtypep '(integer 10) '(rational 11))
  ;; (where the rational is broken up into an integer and a ratio range).
  (loop with surety = t
        with seen-false = nil
        for sct in (junction-ctypes ct2)
        do (multiple-value-bind (val subsurety) (subctypep ct1 sct)
             (cond ((not subsurety) (setf surety nil))
                   (val (return (values t t)))
                   (t
                    (multiple-value-bind (val subsurety) (disjointp ct1 sct)
                      (cond ((not subsurety) (setf seen-false t surety nil))
                            ((not val) (if seen-false
                                           (setf surety nil)
                                           (setf seen-false t))))))))
        finally (return (if (and surety seen-false)
                            (values nil t)
                            (call-next-method)))))

(defmethod disjointp ((ct1 disjunction) (ct2 ctype))
  ;; if a ^ z ~= 0, (a v b) ^ z ~= 0.
  ;; the other way works unless a v b = T.
  ;; Put another way, unless every subtype is disjoint, there's no way
  ;; the whole disjunction is.
  (if (notevery/tri (lambda (sct) (disjointp sct ct2)) (junction-ctypes ct1))
      (values nil t)
      (call-next-method)))
(defmethod disjointp ((ct1 ctype) (ct2 disjunction))
  (if (notevery/tri (lambda (sct) (disjointp ct1 sct)) (junction-ctypes ct2))
      (values nil t)
      (call-next-method)))

(defmethod negate ((ctype disjunction))
  (apply #'conjoin (mapcar #'negate (junction-ctypes ctype))))

(defmethod disjoin/2 ((ct1 disjunction) (ct2 disjunction))
  (apply #'disjoin (append (junction-ctypes ct1)
                           (junction-ctypes ct2))))
(defmethod disjoin/2 ((ct1 disjunction) (ct2 ctype))
  (apply #'disjoin ct2 (junction-ctypes ct1)))
(defmethod disjoin/2 ((ct1 ctype) (ct2 disjunction))
  (apply #'disjoin ct1 (junction-ctypes ct2)))

(defun conjoin-disjunction (disjunction ctype)
  (apply #'disjoin
         (loop for sct in (junction-ctypes disjunction)
               collect (conjoin sct ctype))))
(defmethod conjoin/2 ((ct1 disjunction) (ct2 ctype))
  (conjoin-disjunction ct1 ct2))
(defmethod conjoin/2 ((ct1 ctype) (ct2 disjunction))
  (conjoin-disjunction ct2 ct1))

(defmethod unparse ((ct disjunction))
  (let ((ups (mapcar #'unparse (junction-ctypes ct))))
    ;; special cases
    (when (null ups) (return-from unparse 'nil))
    ;; list
    (when (and (member 'null ups) (member 'cons ups))
      (setf ups (delete 'null ups)
            ups (delete 'cons ups))
      (push 'list ups))
    ;; finally,
    (if (= (length ups) 1)
        (first ups)
        `(or ,@ups))))
