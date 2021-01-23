(in-package #:ctype)

(defmethod ctypep (object (ct disjunction))
  (loop for sct in (junction-ctypes ct)
          thereis (ctypep object sct)))

(defmethod subctypep ((ct1 disjunction) (ct2 ctype))
  ;; if a ~<: z then a v b ~<: z as a <: a v b.
  ;; if a <: z and b <: z, a v b <: z, as can be seen from a <: z <=> a ^ z = a:
  ;; a v b <: z <=> (a v b) ^ z = a v b <=> (a ^ z) v (b ^ z) = a v b
  ;; this also covers the case of ct1 being bot.
  (let ((cts (junction-ctypes ct1)))
    (loop with surety = t
          for sct in cts
          do (multiple-value-bind (val subsurety)
                 (subctypep sct ct2)
               (cond ((not subsurety) (setf surety nil))
                     ((not val) (return (values nil t)))))
          finally (return (if surety (values t t) (call-next-method))))))
(defmethod subctypep ((ct1 ctype) (ct2 disjunction))
  (let ((cts (junction-ctypes ct2)))
    (cond
      ;; if a <: z then a <: z v y as z <: z v y.
      ((loop for sct in cts thereis (subctypep ct1 sct)) (values t t))
      ;; give up
      (t (call-next-method)))))

(macrolet
    ((disjunction-disjointp (disjunction ctype)
       `(let ((cts (junction-ctypes ,disjunction)))
          (if (null cts)
              (values t t)
              ;; if a ^ z != 0 then (a v b) ^ z != 0.
              ;; if a ^ z = 0 and b ^ z = 0 then (a v b) ^ z = 0,
              ;; unless a v b = T.
              (loop with surety = t
                    for sct in cts
                    do (multiple-value-bind (val subsurety)
                           (disjointp sct ,ctype)
                         (cond ((not subsurety) (setf surety nil))
                               ((not val) (return (values nil t)))))
                    finally (return
                              (if surety (values t t) (call-next-method))))))))
  (defmethod disjointp ((ct1 disjunction) (ct2 ctype))
    (disjunction-disjointp ct1 ct2))
  (defmethod disjointp ((ct1 ctype) (ct2 disjunction))
    (disjunction-disjointp ct2 ct1)))

(defmethod negate ((ctype disjunction))
  (if (bot-p ctype)
      (top)
      (apply #'conjoin (mapcar #'negate (junction-ctypes ctype)))))

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
