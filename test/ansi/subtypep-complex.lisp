;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 23 07:12:38 2005
;;;; Contains: Tests of SUBTYPEP on complex types

(in-package #:ctype.test)

(5am:def-suite subtypep.complex :in subtypep)
(5am:in-suite subtypep.complex)

(deftest subtypep-complex.1
  (subtypep* 'complex 'number)
  t t)

(deftest subtypep-complex.2
  (subtypep* 'number 'complex)
  nil t)

(defun check-not-complex-type (type)
  (check-disjointness type 'complex t))

(5am:test subtypep-complex.3
  (mapc #'check-not-complex-type
          '(bit unsigned-byte integer rational ratio real float short-float
                single-float double-float long-float fixnum bignum)))

(5am:test subtypep-complex.4
  (loop for i from 1 to 100
        do (check-not-complex-type `(unsigned-byte ,i))))

(5am:test subtypep-complex.5
  (loop for i from 1 to 100
        do (check-not-complex-type `(signed-byte ,i))))

(5am:test subtypep-complex.7
  (let ((types '(complex (complex) (complex *))))
    (loop for tp1 in types
          do (loop for tp2 in types
                   do (is-subtypep tp1 tp2 t t)))))

(defun check-complex-upgrading (t1 t2)
  (let* ((ucpt1 (upgraded-complex-part-type t1))
         (ucpt2 (upgraded-complex-part-type t2))
         (result (multiple-value-list
                  (subtypep* `(complex ,t1) `(complex ,t2)))))
    (cond
     ((or (equal ucpt1 ucpt2)
          (subtypep t1 t2))
      (unless (equal result '(t t))
        (list (list :case1 t1 t2 ucpt1 ucpt2 result))))
     (t
      (multiple-value-bind
          (ucpt-sub1? good1?)
          (subtypep* ucpt1 ucpt2)
        (multiple-value-bind
            (ucpt-sub2? good2?)
            (subtypep* ucpt2 ucpt1)
          (cond
           ;; the second is not a subtype of the first
           ((and good2? ucpt-sub1? (not ucpt-sub2?))
            (assert good1?)
            (unless (equal result '(nil t))
              (list (list :case2 t1 t2 ucpt1 ucpt2 result))))
           ;; the first is not a subtype of the second
           ((and good1? (not ucpt-sub1?) ucpt-sub2?)
            (assert good2?)
            (unless (equal result '(nil t))
              (list (list :case3 t1 t2 ucpt1 ucpt2 result))))
           ;; they are both subtypes of each other, and so represent
           ;; the same set of objects
           ((and ucpt-sub1? ucpt-sub2?)
            (assert good1?)
            (assert good2?)
            (unless (equal result '(t t))
              (list (list :case4 t1 t2 ucpt1 ucpt2 result)))))))))))

#|
The standard's definition of subtypep on complex types seems broken.
In CLHS COMPLEX it says:

> (complex type-specifier) refers to all complexes that can result from giving numbers of type type-specifier to the function complex, plus all other complexes of the same specialized representation.

So, say (single-float 0.0 1.0) upgrades to single-float, and (float 0.0 1.0) upgrades to float. Then we can pass a (single-float 0.0 1.0) to COMPLEX like (complex 0.5 0.5), and the object we get back is all of a (complex (single-float 0.0 1.0)), (complex single-float), and (complex float). (complex 2.0 2.0) is _also_ a (complex (single-float 0.0 1.0)) if (eql 2.0) upgrades to single-float, since then it's "of the same specialized representation".

In short, it seems like (complex T1) is a subtype of (complex T2) iff T1 is a subtype of T2. However, under subtypep the standard says:

>  For all type-specifiers T1 and T2 other than *,
> (subtypep '(complex T1) '(complex T2)) → true, true
> if:
>    1. T1 is a subtype of T2, or
>    2. (upgraded-complex-part-type 'T1) and (upgraded-complex-part-type 'T2) return two different type specifiers that always refer to the same sets of objects; in this case, (complex T1) and (complex T2) both refer to the same specialized representation.
> The values are false and true otherwise.
> The form
> (subtypep '(complex single-float) '(complex float))
> must return true in all implementations, but
> (subtypep '(array single-float) '(array float))
> returns true only in implementations that do not have a specialized array representation for single floats distinct from that for other floats.

Now this makes no sense to me. In particular point 2 seems to say that if T1 is not a subtype of T2, (complex T1) is only a subtype of (complex T2) if (u-c-p-t T1) and (u-c-p-t T2) return **different specifiers for the same type**. What on earth? Why? Or am I supposed to not take the "different" literally, so (complex T1) is only a subtype of (complex T2) if (u-c-p-t T1) and (u-c-p-t T2) are the **same** type, as with array upgrading? Except that unlike array upgrading, (complex T1) is also a subtype of (complex T2) if T1 is a subtype of T2, so complex types need to track the expressed element type as well.

I don't get it so I'm going to leave this for now.
|#
#+(or)
(deftest subtypep-complex.8
  (let ((types (reverse
                '(bit fixnum bignum integer unsigned-byte rational ratio
                      short-float single-float double-float long-float
                      float real)))
        (float-types
         (remove-duplicates '(short-float single-float double-float long-float)
                            :test #'(lambda (t1 t2)
                                      (eql (coerce 0 t1) (coerce 0 t2))))))
    (loop for i in '(1 2 3 4 6 8 13 16 17 28 29 31 32 48 64)
          do (push `(unsigned-byte ,i) types)
          do (push `(signed-byte ,i) types)
          do (loop for ftp in float-types
                   do (push `(,ftp ,(coerce 0 ftp)
                                   ,(coerce i ftp))
                            types)
                   do (push `(,ftp (,(coerce (- i) ftp))
                                   ,(coerce i ftp))
                            types))
          do (push `(float ,(coerce 0 'single-float)
                           ,(coerce i 'single-float))
                   types))
    (setq types (reverse types))
    (let ((results
           (mapcan #'(lambda (t1)
                       (mapcan #'(lambda (t2) (check-complex-upgrading t1 t2))
                               types))
                   types)))
      (subseq results 0 (min 100 (length results)))))
  nil)
