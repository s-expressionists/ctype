(in-package #:ctype)

(defmethod ctypep (object (ct disjunction))
  (loop for sct in (junction-ctypes ct)
          thereis (ctypep object sct)))

(defmethod subctypep ((ct1 disjunction) (ct2 ctype))
  ;; if a ~<: z then a v b ~<: z as a <: a v b.
  ;; if a <: z and b <: z then a v b <: z.
  (loop with surety = t
        for sct in (junction-ctypes ct1)
        do (multiple-value-bind (val subsurety) (subctypep sct ct2)
             (cond ((not subsurety) (setf surety nil))
                   ((not val) (return (values nil t)))))
        finally (return (if surety (values t t) (call-next-method)))))
(defmethod subctypep ((ct1 ctype) (ct2 disjunction))
  (let ((cts (junction-ctypes ct2)))
    (if (null cts)
        (values (bot-p ct1) t)
        ;; if a <: z then a <: z v y as z <: z v y.
        ;; if a ~<: z and a ~<: y and z v y is not bot then a ~<: z v y.
        ;; I think. Assuming we normalize hard enough.
        (loop with surety = t
              for sct in cts
              do (multiple-value-bind (val subsurety) (subctypep ct1 sct)
                   (cond ((not subsurety) (setf surety nil))
                         (val (return (values t t)))))
              finally (return (if surety (values nil t) (call-next-method)))))))

(macrolet
    ((disjunction-disjointp (disjunction ctype)
       `(let ((cts (junction-ctypes ,disjunction)))
          (if (null cts)
              (values (bot-p ,ctype) t)
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
  (let ((cts (junction-ctypes ct)))
    (if (null cts)
        'nil
        `(or ,@(mapcar #'unparse cts)))))
