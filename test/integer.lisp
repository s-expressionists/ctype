;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:54:05 2003
;;;; Contains: Tests for subtype relationships on integer types

(in-package #:ctype.test)

(5am:def-suite integer :in ctype)
(5am:in-suite integer)

(5am:def-test fixnum-or-bignum ()
  (is-type= '(or fixnum bignum) 'integer))

(5am:def-test fixnum ()
  (is-type= `(integer ,(most-negative-fixnum) ,(most-positive-fixnum))
            'fixnum))

(5am:def-test bignum ()
  (is-type= `(or (integer * (,(most-negative-fixnum)))
                 (integer (,(most-positive-fixnum)) *))
            'bignum))

(5am:def-test integer.subtype.simple () ; originally 1-24
  (is-strict-subtype '(integer 0 10) '(integer 0 20)) ; 1
  (is-strict-subtype '(integer 0 10) '(integer 0 (10)))
  (is-strict-subtype '(integer 10 100) 'integer)
  (is-strict-subtype '(integer 10 100) '(integer))
  (is-strict-subtype '(integer 10 100) '(integer *))
  (is-strict-subtype '(integer 10 100) '(integer * *))
  (is-strict-subtype '(integer 10 *) 'integer) ; 5
  (is-strict-subtype '(integer 10 *) '(integer))
  (is-strict-subtype '(integer 10 *) '(integer *))
  (is-strict-subtype '(integer 10 *) '(integer * *))
  (is-strict-subtype '(integer 10) 'integer) ; 7
  (is-strict-subtype '(integer 10) '(integer))
  (is-strict-subtype '(integer 10) '(integer *))
  (is-strict-subtype '(integer 10) '(integer * *))
  (is-strict-subtype '(integer * 10) 'integer) ; 9
  (is-strict-subtype '(integer * 10) '(integer))
  (is-strict-subtype '(integer * 10) '(integer *))
  (is-strict-subtype '(integer * 10) '(integer * *))
  (is-strict-subtype '(integer 10) '(integer 5)) ; 11
  (is-strict-subtype '(integer 10 *) '(integer 5))
  (is-strict-subtype '(integer 10) '(integer 5 *))
  (is-strict-subtype '(integer 10 *) '(integer 5 *))
  (is-strict-subtype '(integer * 5) '(integer * 10)) ; 19
  (is-unordered '(integer 10 *) '(integer * 10)) ; 22
  (is-disjoint '(integer 10 *) '(integer * (10)))
  (is-type= '(integer (9)) '(integer 10)) ; 23
  (is-type= '(integer * (11)) '(integer * 10)))

(5am:def-test integer.25 ()
  (is-type= '(and (or (integer 0 10) (integer 20 30))
              (or (integer 5 15) (integer 25 35)))
            '(or (integer 5 10) (integer 25 30))))

(5am:def-test integer.conjoin () ; 26
  (is-type= '(and (integer 0 10) (integer 5 15)) '(integer 5 10))
  (is-type= '(and (integer 0 10) (integer 5 10)) '(integer 5 10))
  (is-type= '(and (integer 5 10) (integer 5 15)) '(integer 5 10)))

(5am:def-test integer.disjoin () ; 27
  (is-type= '(or (integer 0 10) (integer 5 15)) '(integer 0 15))
  (is-type= '(or (integer 0 10) (integer 10 15)) '(integer 0 15)))

(5am:def-test integer.hole () ; 28-30
  (is-type= '(and integer (not (integer 10 10)))
            '(or (integer * 9) (integer 11 *)))
  (is-type= '(and integer (not (integer 1 10)))
            '(or (integer * 0) (integer 11 *)))
  (is-type= '(and (integer -100 100) (not (integer 1 10)))
            '(or (integer -100 0) (integer 11 100))))

(5am:def-test integer.hole.eql () ; 28 again
  (is-type= '(and integer (not (eql 10))) '(or (integer * 9) (integer 11 *))))

(5am:def-test integer.rational ()
  (is-type= '(and integer (rational 4 10)) '(integer 4 10))
  (is-type= '(and (integer 4 *) (rational * 10)) '(integer 4 10))
  (is-type= '(and (integer * 10) (rational 4)) '(integer 4 10))
  (are-strict-subtypes (integer (integer) (integer *) (integer * *)) ; 4
      (rational (rational) (rational *) (rational * *)))
  (are-strict-subtypes ((integer 10) (integer 10 *)) ; 5
      (rational (rational) (rational *) (rational * *)
       (rational 19/2) (rational 19/2 *)
       (rational 10) (rational 10 *)))
  (are-strict-subtypes ((integer * 10) (integer * 5)) ; 6
      (rational (rational) (rational *) (rational * *)
       (rational * 21/2)
       (rational * 10) (rational * 1000000000000)))
  (are-strict-subtypes ((integer 0 10) (integer 2 5)) ; 7
      (rational (rational) (rational *) (rational * *)
       (rational * 10) (rational * 1000000000000)
       (rational -1) (rational -1/2)
       (rational -1 *) (rational -1/2 *)
       (rational 0)
       (rational 0 10) (rational * 10)
       (rational 0 *)))
  (is-type= '(and integer (rational (4) 10)) '(integer 5 10))
  (is-type= '(and (integer 4 *) (rational * (10))) '(integer 4 9))
  (is-type= '(and (integer * 10) (rational (4))) '(integer 5 10))
  ;; post-ansi
  (is-type= '(and integer (rational 8/7 35/3)) '(integer 2 11)))

(5am:def-test integer.real ()
  (is-type= '(and integer (real 4 10)) '(integer 4 10))
  (is-type= '(and (integer 4 *) (real * 10)) '(integer 4 10))
  (is-type= '(and (integer * 10) (real 4)) '(integer 4 10))
  (are-strict-subtypes (integer (integer) (integer *) (integer * *))
      (real (real) (real *) (real * *)))
  (are-strict-subtypes ((integer 10) (integer 10 *))
      (real (real) (real *) (real * *)
       (real 10.0) (real 10.0 *) (real 10) (real 10 *)))
  (are-strict-subtypes ((integer * 10) (integer * 5))
      (real (real) (real *) (real * *)
       (real * 10.0) (real * 10) (real * 1000000000000)))
  (are-strict-subtypes ((integer 0 10) (integer 2 5))
      (real (real) (real *) (real * *)
       (real * 10) (real * 1000000000000) (real -10) (real -10.0)
       (real -10 *) (real -10.0 *) (real 0) (real 0.0) (real 0 10)
       (real 0 *)))
  (is-type= '(and (integer 4) (real * 10)) '(integer 4 10))
  (is-type= '(and (integer * 10) (real 4)) '(integer 4 10))
  (is-type= '(and (integer 4) (real * (10))) '(integer 4 9))
  (is-type= '(and (integer * 10) (real (4))) '(integer 5 10)))
