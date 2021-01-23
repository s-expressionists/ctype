(in-package #:ctype)

(defmethod ctypep (object (ct conjunction))
  (loop for sct in (junction-ctypes ct)
        always (ctypep object sct)))

(defmethod subctypep ((ct1 conjunction) (ct2 ctype))
  (or/tri
   ;; if a <: z then a ^ b <: z, as a ^ b <: a.
   ;; unfortunately it does not folow that if a ~<: z and b ~<: z, a ^ b ~<: z,
   ;; because a ^ b could be bot when a and b aren't.
   (some/tri (lambda (sct) (subctypep sct ct2)) (junction-ctypes ct1))
   (call-next-method)))
(defmethod subctypep ((ct1 ctype) (ct2 conjunction))
  ;; if a ~<: z then a ~<: z ^ y, as z ^ y <: z.
  ;; if a <: z and a <: y, a ^ z = a and a ^ y = a
  ;; a <: z ^ y <=> a ^ z ^ y = a <=> (a ^ z) ^ (a ^ y) = a <=> a ^ a = a
  ;; this also covers the case of ct2 being top.
  (surely
   (every/tri (lambda (sct) (subctypep ct1 sct)) (junction-ctypes ct2))
   (call-next-method)))

(defmethod disjointp ((ct1 conjunction) (ct2 ctype))
  ;; if a ^ z = 0 then a ^ b ^ z = 0.
  ;; doesn't follow the other way, though.
  (or/tri
   (some/tri (lambda (sct) (disjointp sct ct2)) (junction-ctypes ct1))
   (call-next-method)))
(defmethod disjointp ((ct1 ctype) (ct2 conjunction))
  (or/tri
   (some/tri (lambda (sct) (disjointp ct1 sct)) (junction-ctypes ct2))
   (call-next-method)))

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
  ;; If any disjunction is uninteresting, give up - except that if some
  ;; of the disjunctions are T, factor those out.
  ;; (This factoring is important for correctly computing that (or x (not x))
  ;;  is top when X involves complicated intersections, for example.)
  ;; This is more complicated than the analogous conjoin-disjunction in order
  ;; to avoid infinite recursion while preferring a sum of products.
  (loop with topseent = nil
        for sct in (junction-ctypes conjunction)
        for dis = (disjoin/2 sct ctype)
        if (not dis)
          collect sct into uninteresting
        else if (top-p dis)
               do (setf topseent t)
        else collect dis into djs
             and collect sct into uninteresting
        finally (return
                  (cond ((null uninteresting) (apply #'conjoin djs))
                        (topseent
                         (disjunction ctype
                                      (apply #'conjunction uninteresting)))
                        (t nil)))))
(defmethod disjoin/2 ((ct1 conjunction) (ct2 ctype))
  (or (disjoin-conjunction ct1 ct2) (call-next-method)))
(defmethod disjoin/2 ((ct1 ctype) (ct2 conjunction))
  (or (disjoin-conjunction ct2 ct1) (call-next-method)))

(defmethod unparse ((ct conjunction))
  (let ((ups (mapcar #'unparse (junction-ctypes ct))))
    ;; Pick off special cases
    (when (null ups) (return-from unparse 't))
    ;; compiled-function
    (when (and (member 'function ups)
               (member '(satisfies compiled-function-p) ups :test #'equal))
      (setf ups (delete 'function ups)
            ups (delete '(satisfies compiled-function-p) ups :test #'equal))
      (push 'compiled-function ups))
    ;; keyword
    (when (and (member 'symbol ups)
               (member '(satisfies keywordp) ups :test #'equal))
      (setf ups (delete 'symbol ups)
            ups (delete '(satisfies keywordp) ups :test #'equal))
      (push 'keyword ups))
    ;; finally,
    (if (= (length ups) 1)
        (first ups)
        `(and ,@ups))))
