(in-package #:ctype)

(defmethod ctypep (object (ct conjunction))
  (loop for sct in (junction-ctypes ct)
        always (ctypep object sct)))

(defmethod subctypep ((ct1 conjunction) (ct2 ctype))
  (let ((cts (junction-ctypes ct1)))
    (if (null cts)
        (values (top-p ct2) t)
        ;; if a <: z then a ^ b <: z, as a ^ b <: a.
        ;; if a ~<: z and b ~<: z and a ^ b is not top then a ^ b ~<: z
        (loop with surety = t
              for sct in cts
              do (multiple-value-bind (val subsurety) (subctypep sct ct2)
                   (cond ((not subsurety) (setf surety nil))
                         (val (return (values t t)))))
              finally (return (if surety (values nil t) (call-next-method)))))))
(defmethod subctypep ((ct1 ctype) (ct2 conjunction))
  ;; if a ~<: z then a ~<: z ^ y, as z ^ y <: z.
  ;; if a <: z and a <: y then a <: z ^ y
  (loop with surety = t
        for sct in (junction-ctypes ct2)
        do (multiple-value-bind (val subsurety) (subctypep ct1 sct)
             (cond ((not subsurety) (setf surety nil))
                   ((not val) (return (values nil t)))))
        finally (return (if surety (values t t) (call-next-method)))))

(defmethod negate ((ctype conjunction))
  (if (top-p ctype)
      (bot)
      ;; de Morgan: ~(a & b) = ~a | ~b
      (apply #'disjoin (mapcar #'negate (junction-ctypes ctype)))))

(defmethod conjoin/2 ((ct1 conjunction) (ct2 conjunction))
  (apply #'conjoin (append (junction-ctypes ct1) (junction-ctypes ct2))))
(defmethod conjoin/2 ((ct1 conjunction) (ct2 ctype))
  (apply #'conjoin ct2 (junction-ctypes ct1)))
(defmethod conjoin/2 ((ct1 ctype) (ct2 conjunction))
  (apply #'conjoin ct1 (junction-ctypes ct2)))

(defun disjoin-conjunction (conjunction ctype)
  (apply #'conjoin
         (loop for sct in (junction-ctypes conjunction)
               collect (disjoin sct ctype))))
(defmethod disjoin/2 ((ct1 conjunction) (ct2 ctype))
  (disjoin-conjunction ct1 ct2))
(defmethod disjoin/2 ((ct1 ctype) (ct2 conjunction))
  (disjoin-conjunction ct2 ct1))

(defmethod unparse ((ct conjunction))
  (let ((cts (junction-ctypes ct)))
    (if (null cts)
        't
        ;; (and x) should be impossible under normalization so don't check.
        `(and ,@(mapcar #'unparse cts)))))
