(in-package #:ctype.test)

(5am:def-suite ctype)

(defun run! (&optional ctype-extrinsic:*client*)
  (5am:run! 'ctype))

(defun subctypep (t1 t2)
  (ctype:subctypep ctype-extrinsic:*client* t1 t2))

(defun ctype= (t1 t2)
  (ctype:ctype= ctype-extrinsic:*client* t1 t2))

(defun disjointp (t1 t2)
  (ctype:disjointp ctype-extrinsic:*client* t1 t2))
(defun conjointp (t1 t2)
  (ctype:conjointp ctype-extrinsic:*client* t1 t2))

(defun most-positive-fixnum ()
  (ctype:most-positive-fixnum ctype-extrinsic:*client*))
(defun most-negative-fixnum ()
  (ctype:most-negative-fixnum ctype-extrinsic:*client*))
(defun upgraded-array-element-type (specifier)
  (ctype:upgraded-array-element-type ctype-extrinsic:*client* specifier))
(defun distinct-zeroes-p (format)
  (ctype:distinct-zeroes-p ctype-extrinsic:*client* format))

(defun specifier-ctype (specifier)
  (ctype-extrinsic:specifier-ctype specifier))

(defmacro %surely-is (op expected string s1 s2 t1 t2)
  `(multiple-value-bind (sub surety) (,op ,t1 ,t2)
     (5am:is (equal '(,expected t) (list sub surety))
             "~s is ~a~a ~s"
             ,s1 ,(if expected
                      `(if surety "not " "not known to be ")
                      `(if surety "" "not known to be not "))
             ,string ,s2)))

(defmacro %surely-ctype= (s1 s2 t1 t2)
  `(%surely-is ctype= t "equal to" ,s1 ,s2 ,t1 ,t2))
(defmacro %surely-not-ctype= (s1 s2 t1 t2)
  `(%surely-is ctype= nil "equal to" ,s1 ,s2 ,t1 ,t2))

(defmacro %surely-subtypep (s1 s2 t1 t2)
  `(%surely-is subctypep t "a subtype of" ,s1 ,s2 ,t1 ,t2))
(defmacro %surely-not-subtypep (s1 s2 t1 t2)
  `(%surely-is subctypep nil "a subtype of" ,s1 ,s2 ,t1 ,t2))

(defmacro %surely-disjointp (s1 s2 t1 t2)
  `(%surely-is disjointp t "disjoint to" ,s1 ,s2 ,t1 ,t2))

(defmacro is-type= (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (%surely-ctype= s1 s2 t1 t2)
       (%surely-subtypep s1 s2 t1 t2)
       (%surely-subtypep s2 s1 t2 t1))))

(defmacro is-subtype (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (%surely-subtypep s1 s2 t1 t2))))

;;; Like IS-SUBTYPEP but allows NIL NIL.
(defmacro is-maybe-subtype (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (5am:is (not (equal '(nil t) (multiple-value-list (subctypep t1 t2))))
               "~s is definitely not a subtype of ~s" s1 s2))))

(defmacro is-maybe-not-subtype (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (5am:is (not (equal '(t t) (multiple-value-list (subctypep t1 t2))))
               "~s is definitely a subtype of ~s" s1 s2))))

(defmacro is-strict-subtype (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (%surely-subtypep s1 s2 t1 t2)
       (%surely-not-subtypep s2 s1 t2 t1))))

;;; Check that ctype doesn't pretend to know anything about the relation.
(defmacro %unsurely-is (op string s1 s2 t1 t2)
  `(multiple-value-bind (sub surety) (,op ,t1 ,t2)
     (5am:is (equal '(nil nil) (list sub surety))
             "~s is ~a~a ~s"
             ,s1 (if sub "" "not ") ,string ,s2)))

(defmacro is-unknown (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (%unsurely-is subctypep "a subtype of" s1 s2 t1 t2)
       (%unsurely-is subctypep "a subtype of" s2 s1 t2 t1)
       (%unsurely-is ctype= "equal to" s1 s2 t1 t2)
       (%unsurely-is disjointp "disjoint to" s1 s2 t1 t2)
       (%unsurely-is conjointp "conjoint to" s1 s2 t1 t2))))

(defmacro are-strict-subtypes ((&rest s1) (&rest s2))
  `(progn
     ,@(loop for sub in s1
             nconc (loop for super in s2
                         collect `(is-strict-subtype ',sub ',super)))))

(defmacro are-type= (&rest specs)
  `(progn ,@(loop for (spec1 . rest) on specs
                  nconc (loop for spec2 in rest
                              collect `(is-type= ',spec1 ',spec2)))))

(defmacro is-unordered (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype ,s1)) (t2 (specifier-ctype ,s2)))
       (%surely-not-subtypep s1 s2 t1 t2)
       (%surely-not-subtypep s2 s1 t2 t1)
       (%surely-not-ctype= s1 s2 t1 t2))))

(defmacro is-disjoint (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype s1)) (t2 (specifier-ctype s2)))
       (%surely-not-subtypep s1 s2 t1 t2)
       (%surely-not-subtypep s2 s1 t2 t1)
       (%surely-not-ctype= s1 s2 t1 t2)
       (%surely-disjointp s1 s2 t1 t2))))
