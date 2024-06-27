(in-package #:ctype)

(defmethod ctypep (client object (ct conjunction))
  (loop for sct in (junction-ctypes ct)
        always (ctypep client object sct)))

(defmethod subctypep (client (ct1 conjunction) (ct2 ctype))
  ;; This is dual to the (ctype disjunction) method in disjunction.lisp.
  ;; Check that comment if you want to see how this works.
  ;; Knowing conjointness is much rarer than knowing disjointness, but does
  ;; happen occasionally; an example is that we can know that
  ;; (subtypep '(and (not integer) (not cons)) 'integer) => NIL, T
  (loop with surety = t
        with not-subtype = 0
        with not-subtype-and-not-conjoint = 0
        for sct in (junction-ctypes ct1)
        do (multiple-value-bind (val subsurety) (subctypep client sct ct2)
             (cond ((not subsurety) (setf surety nil))
                   (val (return (values t t)))
                   (surety
                    (incf not-subtype)
                    (multiple-value-bind (val subsurety)
                        (conjointp client sct ct2)
                      (cond ((not subsurety) (setf surety nil))
                            ((not val) (incf not-subtype-and-not-conjoint)))))))
        finally (return (if (and surety
                                 (or (= not-subtype-and-not-conjoint 1)
                                     (and (> not-subtype 0)
                                          (= not-subtype-and-not-conjoint 0))))
                            (values nil t)
                            (values nil nil)))))
(defmethod subctypep (client (ct1 ctype) (ct2 conjunction))
  ;; if a ~<: z then a ~<: z ^ y, as z ^ y <: z.
  ;; if a <: z and a <: y, a ^ z = a and a ^ y = a
  ;; a <: z ^ y <=> a ^ z ^ y = a <=> (a ^ z) ^ (a ^ y) = a <=> a ^ a = a
  ;; this also covers the case of ct2 being top.
  (every/tri (lambda (sct) (subctypep client ct1 sct)) (junction-ctypes ct2)))

(defmethod disjointp (client (ct1 conjunction) (ct2 conjunction))
  ;; I don't know if there's a better method here, but at least make sure that
  ;; (disjointp t t) => NIL T
  (if (and (null (junction-ctypes ct1)) (null (junction-ctypes ct2)))
      (values nil t)
      (values nil nil)))

(define-commutative-method disjointp (client (ct1 conjunction) (ct2 ctype))
  ;; if a ^ z = 0 then a ^ b ^ z = 0.
  ;; doesn't follow the other way, though.
  (if (some/tri (lambda (sct) (disjointp client sct ct2)) (junction-ctypes ct1))
      (values t t)
      (values nil nil)))

(define-commutative-method conjointp (client (ct1 conjunction) (ct2 ctype))
  ;; (a ^ b) v z = T <=> (a v z) ^ (b v z) = T
  (every/tri (lambda (sct) (conjointp client sct ct2)) (junction-ctypes ct1)))

(defmethod negate (client (ctype conjunction))
  ;; de Morgan: ~(a & b) = ~a | ~b
  (apply #'disjoin client
         (mapcar (lambda (ct) (negate client ct)) (junction-ctypes ctype))))

(defmethod conjoin/2 (client (ct1 conjunction) (ct2 conjunction))
  (apply #'conjoin client (append (junction-ctypes ct1) (junction-ctypes ct2))))
(define-commutative-method conjoin/2 (client (ct1 conjunction) (ct2 ctype))
  (apply #'conjoin client ct2 (junction-ctypes ct1)))

(define-commutative-method disjoin/2
    (client (conjunction conjunction) (ctype ctype))
  ;; If any disjunction is uninteresting, give up - except that if some
  ;; of the disjunctions are T, factor those out.
  ;; (This factoring is important for correctly computing that (or x (not x))
  ;;  is top when X involves complicated intersections, for example.)
  ;; This is more complicated than the analogous conjoin-disjunction in order
  ;; to avoid infinite recursion while preferring a sum of products.
  (loop with topseent = nil
        for sct in (junction-ctypes conjunction)
        for dis = (disjoin/2 client sct ctype)
        if (not dis)
          collect sct into uninteresting
        else if (top-p dis)
               do (setf topseent t)
        else collect dis into djs
             and collect sct into uninteresting
        finally (return
                  (cond ((null uninteresting) (apply #'conjoin client djs))
                        (topseent
                         (disjunction ctype
                                      (apply #'conjunction uninteresting)))
                        (t nil)))))

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
