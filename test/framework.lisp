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

(defun most-positive-fixnum ()
  (ctype:most-positive-fixnum ctype-extrinsic:*client*))
(defun most-negative-fixnum ()
  (ctype:most-negative-fixnum ctype-extrinsic:*client*))

(defun specifier-ctype (specifier)
  (ctype-extrinsic:specifier-ctype specifier))

(defmacro is-type= (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype ,s1)) (t2 (specifier-ctype ,s2)))
       (5am:is (equal '(t t) (multiple-value-list (ctype= t1 t2)))
               "~s is not equal to ~s" s1 s2)
       (5am:is (equal '(t t) (multiple-value-list (subctypep t1 t2)))
               "~s is not a subtype of ~s" s1 s2)
       (5am:is (equal '(t t) (multiple-value-list (subctypep t2 t1)))
               "~s is not a subtype of ~s" s2 s1))))

(defmacro is-strict-subtype (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype ,s1)) (t2 (specifier-ctype ,s2)))
       (5am:is (equal '(t t) (multiple-value-list (subctypep t1 t2)))
               "~s is not a subtype of ~s" s1 s2)
       (5am:is (equal '(nil t) (multiple-value-list (subctypep t2 t1)))
               "~s is a subtype of ~s" s2 s1))))

(defmacro are-strict-subtypes ((&rest s1) (&rest s2))
  `(progn
     ,@(loop for sub in s1
             nconc (loop for super in s2
                         collect `(is-strict-subtype ',sub ',super)))))

(defmacro is-unordered (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype ,s1)) (t2 (specifier-ctype ,s2)))
       (5am:is (equal '(nil t) (multiple-value-list (subctypep t1 t2)))
               "~s is a subtype of ~s" s1 s2)
       (5am:is (equal '(nil t) (multiple-value-list (subctypep t2 t1)))
               "~s is a subtype of ~s" s2 s1)
       (5am:is (equal '(nil t) (multiple-value-list (ctype= t1 t2)))
               "~s is equal to ~s" s1 s2))))

(defmacro is-disjoint (s1 s2)
  `(let ((s1 ,s1) (s2 ,s2))
     (let ((t1 (specifier-ctype ,s1)) (t2 (specifier-ctype ,s2)))
       (5am:is (equal '(nil t) (multiple-value-list (subctypep t1 t2)))
               "~s is a subtype of ~s" s1 s2)
       (5am:is (equal '(nil t) (multiple-value-list (subctypep t2 t1)))
               "~s is a subtype of ~s" s2 s1)
       (5am:is (equal '(nil t) (multiple-value-list (ctype= t1 t2)))
               "~s is equal to ~s" s1 s2)
       (5am:is (equal '(t t) (multiple-value-list (disjointp t1 t2)))
               "~s is not disjoint from ~s" s1 s2))))
