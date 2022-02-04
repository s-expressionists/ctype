(in-package #:ctype.ext.tfun)

(defmacro with-ranges ((&rest bindings) &body body)
  (loop for ((low lxp high hxp) rangeform) in bindings
        for rsym = (gensym "RANGE")
        collect `(,rsym ,rangeform) into rbindings
        collect `(,low (range-low ,rsym)) into rbindings
        collect `(,lxp (range-low-exclusive-p ,rsym)) into rbindings
        collect `(,high (range-high ,rsym)) into rbindings
        collect `(,hxp (range-high-exclusive-p ,rsym)) into rbindings
        finally (return `(let* (,@rbindings) ,@body))))

(defun all-rest-values (values-ctype)
  (list* (cvalues-rest values-ctype)
         (append (cvalues-required values-ctype)
                 (cvalues-optional values-ctype))))

;;; FIXME: In various places below we check constantness, but we could be a
;;; little bit looser. e.g. = on (or (eql 4) (eql 4.0))s is still true.

;; Are the values of t1 and t2 provably never =?
(defgeneric disjoint-=-p (t1 t2)
  (:method-combination basic surely)
  (:method ((ct1 ctype) (ct2 ctype)) (values nil nil)))
(defmethod disjoint-=-p ((t1 conjunction) (t2 ctype))
  (some/tri (lambda (ty) (disjoint-=-p ty t2)) (junction-ctypes t1)))
(defmethod disjoint-=-p ((t1 ctype) (t2 conjunction))
  (some/tri (lambda (ty) (disjoint-=-p t1 ty)) (junction-ctypes t2)))
(defmethod disjoint-=-p ((t1 disjunction) (t2 ctype))
  (every/tri (lambda (ty) (disjoint-=-p ty t2)) (junction-ctypes t1)))
(defmethod disjoint-=-p ((t1 ctype) (t2 disjunction))
  (every/tri (lambda (ty) (disjoint-=-p t1 ty)) (junction-ctypes t2)))
(defun ranges-disjoint-p (low1 lxp1 high1 hxp1 low2 lxp2 high2 hxp2)
  (or (and high1 low2
           (or (< high1 low2) (and (= high1 low2) (or hxp1 lxp2))))
      (and high2 low1
           (or (< high2 low1) (and (= high2 low1) (or hxp2 lxp1))))))
(defmethod disjoint-=-p ((t1 range) (t2 range))
  ;; We can't just rely on disjointp since = can compare numbers
  ;; of different kind.
  (ranges-disjoint-p (range-low t1) (range-low-exclusive-p t1)
                     (range-high t1) (range-high-exclusive-p t1)
                     (range-low t2) (range-low-exclusive-p t2)
                     (range-high t2) (range-high-exclusive-p t2)))

(defun some-pairwise (function sequence)
  (let ((len (length sequence)))
    (loop for i below len
          for ei = (elt sequence i)
            thereis (loop for j from (1+ i) below len
                          for ej = (elt sequence j)
                            thereis (funcall function ei ej)))))

(define-tfun = (num1 &rest tail)
  (let ((tys (all-rest-values tail))
        (false (specifier-ctype 'null)))
    (single-value
     (cond
       ;; If some pair of types is disjoint, always false.
       ((some-pairwise #'disjoint-=-p tys) false)
       ;; If every type is a constant, e.z.
       ((every #'constant-type-p tys)
        (if (apply #'= (mapcar #'constant-type-value tys))
            (negate false)
            false))
       ;; If some types are constant we may still eke out a false.
       ((some #'constant-type-p tys)
        (let* ((constys (remove-if-not #'constant-type-p tys))
               (consts (mapcar #'constant-type-value constys)))
          (if (not (apply #'= consts))
              false
              (top))))
       ;; who knows
       (t (top))))))

;;; This is used on ordering functions like <.
;;; It calls the function on each successive pair of types.
;;; The function is expected to have subtypep return convention.
;;; ORDER will then return true iff all the pairs return true.
;;; If some pair returns false it will immediately return false,
;;; and then it will do a more exhaustive pairwise search just in case.
;;; so that, e.g.,
;;; (< (the (integer 10 11) x) (the integer y) (the (integer 0 5) z)) is caught
;;; Failing that, it gives up and returns unknown.
(defun order (function types)
  (loop with total-surety = t
        for stypes on types
        while (consp (cdr stypes))
        do (let ((t1 (first stypes)) (t2 (second stypes)))
             (multiple-value-bind (val surety) (funcall function t1 t2)
               (cond (val)
                     (surety (return-from order (values nil t)))
                     (t (setf total-surety nil)))))
        finally (when total-surety (return-from order (values t t))))
  ;; OK, backup check.
  (loop for (t1 . stypes) on types
        while (consp (cdr stypes))
        do (let (;; skip (first stypes) since we did that in the prev loop.
                 (t2 (second stypes)))
             (multiple-value-bind (val surety) (funcall function t1 t2)
               (when (and surety (not val))
                 (return-from order (values nil t))))))
  ;; Give up.
  (values nil nil))

;;; ORDER2 is like ORDER above, but handles a &rest argument type.
(defun order2 (function types tail &optional (reflexivep t))
  (let ((false (specifier-ctype 'null))
        (tailp (not (bot-p tail))))
    (multiple-value-bind (value surety) (order function types)
      (cond (value
             (if tailp
                 ;; can't be sure about the tail,
                 ;; except that if it's a constant somehow we know false.
                 ;; (e.g. (apply #'< (the (list-of integer) x)) could be
                 ;;  anything)
                 (if (and (not reflexivep) (constant-type-p tail))
                     false
                     (top))
                 (negate false)))
            (surety false)
            (t
             (if tailp
                 ;; It's possible we could prove false from the tail type
                 ;; e.g. (apply #'< 8 (the (list-of bit) y))
                 (if (some (lambda (ty)
                             (multiple-value-bind (v s)
                                 (funcall function ty tail)
                               (and s (not v))))
                           types)
                     false
                     (top))))))))

(defun compare-conjunction (function conjunction ctype)
  ;; this is easy: if any of the supertypes of the conjunction have a clear
  ;; answer, so do we.
  (loop for ty in (junction-ctypes conjunction)
        do (multiple-value-bind (v s) (funcall function ty ctype)
             (when s (return-from compare-conjunction (values v s)))))
  (values nil nil))
(defun compare-disjunction (function disjunction ctype)
  ;; if every subtype of the disjunction has a clear answer, and they all
  ;; agree on that answer, we can use that.
  ;; I think?
  (loop with surety = t
        with value = :unknown
        for ty in (junction-ctypes disjunction)
        do (multiple-value-bind (v s) (funcall function ty ctype)
             (when s
               (cond ((eq value :unknown) (setf value v))
                     ((eq value v))
                     (t (return-from compare-disjunction
                          (values nil nil))))))
        finally (assert (member value '(t nil)))
                (return (values value surety))))

(declaim (inline flip))
(defun flip (f) (lambda (t2 t1) (funcall f t1 t2)))

(defgeneric t<  (t1 t2))
(defgeneric t>  (t1 t2))
(defgeneric t<= (t1 t2))
(defgeneric t>= (t1 t2))
(defmethod t< ((t1 range) (t2 range))
  (with-ranges (((low1 lxp1 high1 hxp1) t1)
                ((low2 lxp2 high2 hxp2) t2))
    (declare (ignore lxp1 lxp2 hxp2))
    (cond ((and high1 low2 (< high1 low2)) (values t t))
          ((and low1 high2 (<= low1 high2)) (values nil t))
          ((and high1 low2 (= high1 low2) hxp1) (values t t))
          (t (values nil nil)))))
(defmethod t> ((t1 range) (t2 range))
  (with-ranges (((low1 lxp1 high1 hxp1) t1)
                ((low2 lxp2 high2 hxp2) t2))
    (declare (ignore lxp1 lxp2 hxp1))
    (cond ((and high2 low1 (< high2 low1)) (values t t))
          ((and low2 high1 (<= low2 high1)) (values nil t))
          ((and high2 low1 (= high2 low1) hxp2) (values t t))
          (t (values nil nil)))))
(defmethod t<= ((t1 range) (t2 range))
  (with-ranges (((low1 lxp1 high1 hxp1) t1)
                ((low2 lxp2 high2 hxp2) t2))
    (declare (ignore lxp2 hxp1))
    (cond ((and high1 low2 (<= high1 low2)) (values t t))
          ((and low1 high2 (< low1 high2)) (values nil t))
          ((and low1 high2 (= low1 high2) (or lxp1 hxp2)) (values nil t))
          (t (values nil nil)))))
(defmethod t>= ((t1 range) (t2 range))
  (with-ranges (((low1 lxp1 high1 hxp1) t1)
                ((low2 lxp2 high2 hxp2) t2))
    (declare (ignore lxp1 hxp2))
    (cond ((and high2 low1 (<= high2 low1)) (values t t))
          ((and low2 high1 (< low2 high1)) (values nil t))
          ((and low2 high1 (= low2 high1) (or lxp2 hxp1)) (values nil t))
          (t (values nil nil)))))
(macrolet ((defdefault (fname)
             `(progn
                (defmethod ,fname ((t1 ctype) (t2 ctype)) (values nil nil))
                (defmethod ,fname ((t1 conjunction) (t2 ctype))
                  (compare-conjunction #',fname t1 t2))
                (defmethod ,fname ((t1 ctype) (t2 conjunction))
                  (compare-conjunction (flip #',fname) t2 t1))
                (defmethod ,fname ((t1 disjunction) (t2 ctype))
                  (compare-disjunction #',fname t1 t2))
                (defmethod ,fname ((t1 ctype) (t2 disjunction))
                  (compare-disjunction (flip #',fname) t2 t1))))
           (defdefaults (&rest fnames)
             `(progn ,@(loop for fn in fnames collect `(defdefault ,fn)))))
  (defdefaults t< t> t<= t>=))

(define-tfun < (num1 &rest tail)
  (let ((types (list* num1 (append (cvalues-required tail)
                                   (cvalues-optional tail))))
        (tail (cvalues-rest tail)))
    (single-value (order2 #'t< types tail nil))))
(define-tfun > (num1 &rest tail)
  (let ((types (list* num1 (append (cvalues-required tail)
                                   (cvalues-optional tail))))
        (tail (cvalues-rest tail)))
    (single-value (order2 #'t> types tail nil))))
(define-tfun <= (num1 &rest tail)
  (let ((types (list* num1 (append (cvalues-required tail)
                                   (cvalues-optional tail))))
        (tail (cvalues-rest tail)))
    (single-value (order2 #'t<= types tail t))))
(define-tfun >= (num1 &rest tail)
  (let ((types (list* num1 (append (cvalues-required tail)
                                   (cvalues-optional tail))))
        (tail (cvalues-rest tail)))
    (single-value (order2 #'t>= types tail t))))

(defun contagion (kind1 kind2)
  (ecase kind1
    ((integer)
     (case kind2
       ((integer) kind2)
       ((ratio) 'rational)
       (t kind2)))
    ((ratio)
     (case kind2
       ((integer ratio) 'rational)
       (t kind2)))
    ((short-float)
     (case kind2
       ((integer ratio) kind1)
       (t kind2)))
    ((single-float)
     (case kind2
       ((integer ratio short-float) kind1)
       (t kind2)))
    ((double-float)
     (case kind2
       ((integer ratio short-float single-float) kind1)
       (t kind2)))
    ((long-float) kind1)))

(defun rational-range (low lxp high hxp)
  (specifier-ctype
   `(rational ,(if (and lxp low) `(,low) low)
              ,(if (and hxp high) `(,high) high))))

;;; like RANGE, but accepts contagion results (RATIONAL is the weird one).
(defun contagion-range (kind low lxp high hxp)
  (if (eq kind 'rational)
      (rational-range low lxp high hxp)
      (range kind low lxp high hxp)))

(defgeneric t+ (t1 t2))
(defdefaults t+ (num1 num2) (specifier-ctype 'number))
(defmethod t+ ((t1 range) (t2 range))
  (with-ranges (((low1 lxp1 high1 hxp1) t1)
                ((low2 lxp2 high2 hxp2) t2))
    (let ((kind (contagion (range-kind t1) (range-kind t2)))
          (low (if (and low1 low2)
                   (+ low1 low2)
                   ;; negative infinity
                   nil))
          (lxp (or lxp1 lxp2))
          (high (if (and high1 high2)
                    (+ high1 high2)
                    nil))
          (hxp (or hxp1 hxp2)))
      (contagion-range kind low lxp high hxp))))

;;; What is the type of (apply #'+ (list-of x))?
;;; If x is nil (i.e. the list is zero length), the result is 0, so (eql 0)
;;; is in there no matter what.
;;; Otherwise, if the lower bound of x is at least 0, the result has
;;; the same lower bound. And similarly with signs flipped.
(defgeneric t+exp (type))
(defmethod t+exp ((type ctype)) (specifier-ctype 'number))
(defmethod t+exp :around ((type ctype))
  (disjoin
   (range 'integer 0 nil 0 nil)
   (conjoin (call-next-method) (specifier-ctype 'number))))
(defmethod t+exp ((type conjunction))
  (apply #'conjoin (mapcar #'t+exp (junction-ctypes type))))
(defmethod t+exp ((type disjunction))
  (apply #'disjoin (mapcar #'t+exp (junction-ctypes type))))
(defmethod t+exp ((type range))
  (let ((low (range-low type)) (high (range-high type))
        (lxp (range-low-exclusive-p type)) (hxp (range-high-exclusive-p type))
        (kind (range-kind type)))
    (multiple-value-bind (low lxp)
        (if (and low (>= low 0))
            (values low lxp)
            (values nil nil))
      (multiple-value-bind (high hxp)
          (if (and high (<= high 0))
              (values high hxp)
              (values nil nil))
        (range kind low lxp high hxp)))))

(defun ts+ (tail)
  (let* ((req (cvalues-required tail))
         (opt (cvalues-optional tail))
         (rest (cvalues-rest tail))
         (tail (t+exp rest))
         (sub (reduce #'t+ req :initial-value tail)))
    (apply #'disjoin
           sub
           (loop for o in opt
                 for s = sub then lastsub
                 for lastsub = (t+ o s)
                 collect lastsub))))

(define-tfun + (&rest tail) (single-value (ts+ tail)))

(defgeneric tneg (type)) ; arithmetic negation
(defdefaults tneg (number) (specifier-ctype 'number))
(defmethod tneg ((type negation))
  (negate (tneg (negation-ctype type))))
(defmethod tneg ((type range))
  (let ((low (range-low type)) (high (range-high type))
        (lxp (range-low-exclusive-p type))
        (hxp (range-low-exclusive-p type))
        (kind (range-kind type)))
    (range kind (if high (- high) nil) hxp (if low (- low) nil) lxp)))

(define-tfun - (minuend &rest subtrahends)
  (single-value
   ;; - is a bit weird in doing completely different operations depending on
   ;; how many arguments it gets. We distinguish them as best we can.
   (cond ((cvalues-required subtrahends) (t+ minuend (tneg (ts+ subtrahends))))
         ((values-bot-p subtrahends) (tneg minuend))
         (t (disjoin (t+ minuend (tneg (ts+ subtrahends)))
                     (tneg minuend))))))

(defgeneric t*exp (type))
(defmethod t*exp ((type ctype)) (specifier-ctype 'number))
(defmethod t*exp :around ((type ctype))
  (disjoin
   (range 'integer 1 nil 1 nil)
   (conjoin (call-next-method) (specifier-ctype 'number))))
(defmethod t*exp ((type conjunction))
  (apply #'conjoin (mapcar #'t+exp (junction-ctypes type))))
(defmethod t*exp ((type disjunction))
  (apply #'disjoin (mapcar #'t+exp (junction-ctypes type))))
(defmethod t*exp ((type range))
  (let ((low (range-low type)) (high (range-high type))
        (lxp (range-low-exclusive-p type)) (hxp (range-high-exclusive-p type))
        (kind (range-kind type)))
    (cond ((and low (>= low 0))
           ;; Input is bounded below nonnegatively, so the result is too
           (range kind low lxp nil nil))
          ((and high (<= high 0))
           ;; Input is bounded above negatively. As such we know that
           ;; its odd integer powers are less than that bound, and its even
           ;; integer powers are at least that bound squared.
           ;; e.g. powers of (integer * -7) are
           ;; (or (integer * -7) (integer 49 *))
           (disjoin (range kind nil nil high hxp)
                    (range kind (* high high) hxp nil nil)))
          ((and low (>= low -1) high (<= high 1))
           ;; Magnitude is <= 1, so multiplication doesn't grow.
           type)
          (t (range kind nil nil nil nil)))))

(defgeneric t* (t1 t2))
(defdefaults t* (t1 t2) (specifier-ctype 'number))
(defun multiply-bound (bound1 xp1 bound2 xp2)
  (case bound1
    ((:-infinity)
     (case bound2
       ((:-infinity) (values :+infinity nil))
       ((:+infinity) (values :-infinity nil))
       (t (cond ((zerop bound2) (values bound2 xp2))
                ((minusp bound2) (values :+infinity nil))
                (t (values :-infinity nil))))))
    ((:+infinity)
     (case bound2
       ((:-infinity) (values :-infinity nil))
       ((:+infinity) (values :+infinity nil))
       (t (cond ((zerop bound2) (values bound2 xp2))
                ((minusp bound2) (values :-infinity nil))
                (t (values :+infinity nil))))))
    (t
     (case bound2
       ((:-infinity)
        (cond ((zerop bound1) (values bound1 xp1))
              ((minusp bound1) (values :+infinity nil))
              (t (values :-infinity nil))))
       ((:+infinity)
        (cond ((zerop bound1) (values bound1 xp1))
              ((minusp bound1) (values :-infinity nil))
              (t (values :+infinity nil))))
       (t (values (* bound1 bound2) (or xp1 xp2)))))))
(defun bound< (bound1 xp1 bound2 xp2)
  (case bound1
    ((:-infinity) t)
    ((:+infinity) nil)
    (t (case bound2
         ((:-infinity) nil)
         ((:+infinity) t)
         (t (cond ((< bound1 bound2) t)
                  ;; not sure about symmetry here
                  ((= bound1 bound2) (or xp1 (not xp2)))
                  (t nil)))))))
;; Get the least and greatest bounds out of four.
;; Thank you, wikipedia article on sorting networks
(defun bound-minmax4 (bound1 bxp1 bound2 bxp2
                      bound3 bxp3 bound4 bxp4)
  (unless (bound< bound1 bxp1 bound3 bxp3)
    (rotatef (values bound1 bxp1) (values bound3 bxp3)))
  (unless (bound< bound2 bxp2 bound4 bxp4)
    (rotatef (values bound2 bxp2) (values bound4 bxp4)))
  (unless (bound< bound1 bxp1 bound2 bxp2)
    (rotatef (values bound1 bxp1) (values bound2 bxp2)))
  (unless (bound< bound3 bxp3 bound4 bxp4)
    (rotatef (values bound3 bxp3) (values bound4 bxp4)))
  (values bound1 bxp1 bound4 bxp4))
(defmethod t* ((t1 range) (t2 range))
  (let ((low1 (or (range-low t1) :-infinity))
        (high1 (or (range-high t1) :+infinity))
        (lxp1 (range-low-exclusive-p t1))
        (hxp1 (range-low-exclusive-p t2))
        (low2 (or (range-low t2) :-infinity))
        (high2 (or (range-high t2) :+infinity))
        (lxp2 (range-low-exclusive-p t2))
        (hxp2 (range-low-exclusive-p t2)))
    (multiple-value-bind (low lxp high hxp)
        (multiple-value-call #'bound-minmax4
          (multiply-bound low1 lxp1 low2 lxp2)
          (multiply-bound low1 lxp1 high2 hxp2)
          (multiply-bound high1 hxp1 low2 lxp2)
          (multiply-bound high1 hxp1 high2 hxp2))
      (let ((low (if (eq low :-infinity) nil low))
            (high (if (eq high :+infinity) nil high)))
        (range (contagion (range-kind t1) (range-kind t2))
               low lxp high hxp)))))

(defun ts* (tail)
  (let* ((req (cvalues-required tail))
         (opt (cvalues-optional tail))
         (rest (cvalues-rest tail))
         (tail (t*exp rest))
         (sub (reduce #'t* req :initial-value tail)))
    (apply #'disjoin
           sub
           (loop for o in opt
                 for s = sub then lastsub
                 for lastsub = (t* o s)
                 collect lastsub))))

(define-tfun * (&rest multiplicands) (single-value (ts* multiplicands)))

(defgeneric tinv (type)) ; arithmetic inverse
(defdefaults tinv (number) (specifier-ctype 'number))
(defmethod tinv ((type negation))
  (negate (tinv (negation-ctype type))))
(defmethod tinv ((type range))
  (let* ((low (range-low type)) (high (range-high type))
         (lxp (range-low-exclusive-p type)) (hxp (range-high-exclusive-p type))
         (kind (range-kind type))
         (zero (case kind ((integer ratio) 0) (t (coerce 0 kind)))))
    (multiple-value-bind (ilow ilxp)
        (cond ((and high (< high 0)) (values (/ high) hxp))
              ;; nother UB carefulness issue here FIXME
              ((and high (= high 0)) (values nil nil))
              ;; we now know high is positive (possibly infinite)
              ;; as such, if low is negative, we cross zero, so the result
              ;; low bound is negative infinity
              ((or (not low) (< low 0)) (values nil lxp))
              (high (values (/ high) hxp))
              ;; high is infinite and low is nonnegative
              (t (values zero t)))
      (multiple-value-bind (ihigh ihxp)
          (cond ((and low (> low 0)) (values (/ low) lxp))
                ((and low (= low 0)) (values nil nil))
                ;; low is negative
                ((or (not high) (> high 0)) (values nil nil)) ; zero crossing
                (low (values (/ low) lxp))
                (t (values zero t)))
        (case kind
          ((integer ratio) (rational-range ilow ilxp ihigh ihxp))
          (t (range kind ilow ilxp ihigh ihxp)))))))

(define-tfun / (numerator &rest denominators)
  (single-value
   (cond ((cvalues-required denominators)
          (t* numerator (tinv (ts* denominators))))
         ((values-bot-p denominators) (tinv numerator))
         (t (disjoin (t* numerator (tinv (ts* denominators)))
                     (tinv numerator))))))

(define-tfun 1+ (number)
  (single-value (t+ number (range 'integer  1 nil  1 nil))))
(define-tfun 1- (number)
  (single-value (t+ number (range 'integer -1 nil -1 nil))))

(defgeneric tabs (number))
(defdefaults tabs (number) (specifier-ctype '(real 0)))
(defmethod tabs ((type range))
  (let* ((low (range-low type)) (high (range-high type))
         (lxp (range-low-exclusive-p type)) (hxp (range-high-exclusive-p type))
         (kind (range-kind type)))
    (if (or (null low) (< low 0))
        (multiple-value-bind (high hxp)
            (if (or (null high) (null low))
                (values nil nil)
                (let ((ahigh (abs high)) (alow (abs low)))
                  (cond ((< ahigh alow) (values alow lxp))
                        ((> ahigh alow) (values ahigh hxp))
                        (t (values ahigh (and lxp hxp))))))
          (case kind
            ((ratio) (range kind 0 t high hxp))
            (t (range kind (coerce 0 kind) nil high hxp))))
        type)))

(define-tfun abs (number) (single-value (tabs number)))

;;; exponentiation
(defgeneric texp (type))
(defdefaults texp (number) (specifier-ctype 'number))
;; Due to rational/float substitutability, we can't validly do negations.
;; e.g., (texp '(integer 1 7)) is (single-float 2.71... 1096...).
;; but (texp '(not (integer 1 7))) also includes (single-float 2.71... 1096...)
;; because the argument could be a single float.
(defmethod texp ((type range))
  (let* ((low (range-low type)) (high (range-high type))
         (lxp (range-low-exclusive-p type)) (hxp (range-high-exclusive-p type))
         (kind (range-kind type))
         (int1p nil))
    ;; (exp rational) is never rational, so it must return single float
    ;; ...except (exp 0), which could be 1. So check for that first
    ;; NOTE: Behavior of float substitutability etc should be controllable
    ;; with a flag or something.
    (case kind
      ((integer)
       (when (and (or (not high) (> high 0) (and (= high 0) (not hxp)))
                  (or (not low) (< low 0) (and (= low 0) (not lxp))))
         ;; 0 included
         (setf int1p t))))
    (case kind
      ((integer ratio)
       (setf kind 'single-float
             low (if low (float low) low)
             high (if high (float high) high))))
    (multiple-value-bind (low lxp)
        (if low
            (values (exp low) lxp)
            (values (coerce 0 kind) t))
      (multiple-value-bind (high hxp)
          (if high
              (handler-case
                  (values (exp high) hxp)
                (floating-point-overflow () (values nil nil)))
              (values nil nil))
        (let ((range (range kind low lxp high hxp)))
          (if int1p
              (disjoin range (range 'integer 1 nil 1 nil))
              range))))))

(define-tfun exp (number) (single-value (texp number)))

(defgeneric trandom (real))
(defdefaults trandom (real) (specifier-ctype '(real 0)))
(defmethod trandom ((real range))
  (let ((high (range-high real)) (kind (range-kind real)))
    (when (eq kind 'ratio) (return-from trandom (bot)))
    (when (and high (<= high 0)) (return-from trandom (bot)))
    (range kind (coerce 0 kind) nil high (if high t nil))))

(define-tfun random (real &optional random-state)
  (single-value (trandom real)))

(defun %float-proto (real protok)
  (let ((low (range-low real)) (lxp (range-low-exclusive-p real))
        (high (range-high real)) (hxp (range-high-exclusive-p real)))
    (when (member protok '(integer ratio))
      (return-from %float-proto (bot)))
    (multiple-value-bind (low lxp)
        (if low (values (coerce low protok) lxp) (values nil nil))
      (multiple-value-bind (high hxp)
          (if high (values (coerce high protok) hxp) (values nil nil))
        (range protok low lxp high hxp)))))

(defgeneric float-proto (real proto))
(defdefaults float-proto (real proto) (specifier-ctype 'float))
(defmethod float-proto ((real range) (proto range))
  (%float-proto real (range-kind proto)))

(defun float-noproto (real) (%float-proto real 'single-float))

(define-tfun float (real &rest rest)
  (single-value
   (let ((req (cvalues-required rest)) (opt (cvalues-optional rest))
         (rest (cvalues-rest rest)))
     (cond ((> (length req) 1) (bot))
           (req (float-proto real (first req)))
           ((bot-p (or (first opt) rest)) (float-noproto real))
           (t (disjoin (float-proto real (or (first req) (first opt) rest))
                       (float-noproto real)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Integer functions
;;;

(defgeneric tash (integer-type count-type))
(defdefaults tash (integer count) (range 'integer nil nil nil nil))
(defmethod tash ((itype range) (ctype range))
  (unless (and (eq (range-kind itype) 'integer)
               (eq (range-kind ctype) 'integer))
    (return-from tash (bot)))
  (let (;; we rely on exclusivep being false for integers.
        (ilow (range-low itype)) (ihigh (range-high itype))
        (clow (range-low ctype)) (chigh (range-high ctype)))
    ;; ash with a positive count increases magnitude
    ;; while ash with a negative count decreases it.
    ;; therefore: if the integer can be negative, the low point of the range
    ;; must be (ash ilow chigh). even in the case of chigh being negative,
    ;; this can at worst result in 0, which is <= any lower bound from ihigh.
    ;; if it can't be negative, low bound must be (ash ilow clow).
    ;; high bound should work similarly.
    (let ((low
            (cond ((not ilow) nil)
                  ((< ilow 0) (if chigh (ash ilow chigh) nil))
                  ((> ilow 0) (if clow (ash ilow clow) 0))
                  (t 0)))
          (high
            (cond ((not ihigh) nil)
                  ((< ihigh 0) (if clow (ash ihigh clow) 0))
                  ((> ihigh 0) (if chigh (ash ihigh chigh) nil))
                  (t 0))))
      (range 'integer low nil high nil))))

(define-tfun ash (integer count) (single-value (tash integer count)))

(defgeneric tinteger-length (type))
(defdefaults tinteger-length (integer) (range 'integer 0 nil nil nil))
(defmethod tinteger-length ((type range))
  (unless (eq (range-kind type) 'integer)
    (return-from tinteger-length (bot)))
  ;; integer-length is monotonic with respect to magnitude,
  ;; so the high is just (max (integer-length low) (integer-length high)).
  ;; the low is a bit trickier - it's the min of the extremum lengths,
  ;; except that if the range includes zero it's zero.
  (let ((low (range-low type)) (high (range-high type)))
    (let ((high (if (and low high)
                    (max (integer-length low) (integer-length high))
                    nil))
          (low (cond ((not low)
                      (if (or (not high) (> high 0)) 0 (integer-length high)))
                     ((not high)
                      (if (< low 0) 0 (integer-length low)))
                     (t
                      (if (and (<= low 0) (>= high 0))
                          0
                          (min (integer-length low) (integer-length high)))))))
      (range 'integer low nil high nil))))

(define-tfun integer-length (integer) (single-value (tinteger-length integer)))

(defgeneric tlogcount (type))
(defdefaults tlogcount (integer) (range 'integer 0 nil nil nil))
(defmethod tlogcount ((type range))
  (unless (eq (range-kind type) 'integer)
    (return-from tlogcount (bot)))
  (let ((low (range-low type)) (high (range-high type)))
    ;; This could be more exact sometimes (most obviously, for constants).
    ;; We try to at least exclude zero when possible.
    (let ((low (if (or (and low (> low 0)) (and high (< high -1))) 1 0))
          (high (if (and high low)
                    (max (integer-length high) (integer-length low))
                    nil)))
      (range 'integer low nil high nil))))

(define-tfun logcount (integer) (single-value (tlogcount integer)))
