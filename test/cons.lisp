;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:57:03 2003
;;;; Contains: Tests for subtype relationships on cons types
;;;; Adapted from ANSI tests' subtypep-cons.lsp.

(in-package #:ctype.test)

(5am:def-suite cons :in ctype)
(5am:in-suite cons)

(5am:def-test cons.id () ; subtypep.cons.1
  (are-type= cons (cons) (cons *) (cons * *) (cons t) (cons t t)
             (cons t *) (cons * t)))

(5am:def-test cons.bottom () ; subtypep.cons.2, 35-37
  (macrolet ((are-bot (&rest specs)
               `(progn ,@(loop for spec in specs
                               collect `(is-subtype ',spec 'nil)))))
    (are-bot (cons nil) (cons nil *) (cons nil t)
             (cons * nil) (cons t nil) (cons nil nil)))
  (is-strict-subtype '(cons nil t) 'float) ; 35
  (is-strict-subtype '(cons t nil) 'symbol)
  (is-strict-subtype '(cons nil nil) 'real))

(5am:def-test cons.conjoin-carcdr () ; subtypep.cons.3
  (is-type= '(and (cons symbol *) (cons * symbol)) '(cons symbol symbol)))

(5am:def-test cons.sets () ; subtypep.cons.4-41
  (is-type= '(and (cons (integer 0 10) *)
              (cons (integer 5 15) (integer 10 20))
              (cons * (integer 15 25)))
            '(cons (integer 5 10) (integer 15 20)))
  (is-type= '(and cons (not (cons symbol symbol)))
            '(or (cons (not symbol) *) (cons * (not symbol))))
  (is-type= '(or (cons integer symbol) (cons integer integer)
              (cons symbol integer) (cons symbol symbol))
            '(cons (or integer symbol) (or integer symbol)))
  (is-type= '(or (cons (integer 0 8) (integer 5 15))
              (cons (integer 0 7) (integer 0 6))
              (cons (integer 6 15) (integer 0 9))
              (cons (integer 3 15) (integer 4 15)))
            '(cons (integer 0 15) (integer 0 15)))
  (is-type= '(or
              (cons integer (cons symbol integer))
              (cons symbol (cons integer symbol))
              (cons symbol (cons symbol integer))
              (cons symbol (cons integer integer))
              (cons integer (cons integer symbol))
              (cons symbol (cons symbol symbol))
              (cons integer (cons integer integer))
              (cons integer (cons symbol symbol)))
            '(cons (or symbol integer)
              (cons (or symbol integer) (or symbol integer))))
  (is-type= '(or
              (cons (integer 0 (3)) (integer 0 (6)))
              (cons (integer 3 (9)) (integer 0 (3)))
              (cons (integer 0 (6)) (integer 6 (9)))
              (cons (integer 6 (9)) (integer 3 (9)))
              (cons (integer 3 (6)) (integer 3 (6))))
            '(cons (integer 0 (9)) (integer 0 (9))))
  (is-type= '(or ; 10
              (cons (real 0 (3)) (real 0 (6)))
              (cons (real 3 (9)) (real 0 (3)))
              (cons (real 0 (6)) (real 6 (9)))
              (cons (real 6 (9)) (real 3 (9)))
              (cons (real 3 (6)) (real 3 (6))))
            '(cons (real 0 (9)) (real 0 (9))))
  (is-strict-subtype '(or (cons integer symbol) (cons symbol integer))
                     '(cons (or integer symbol) (or integer symbol)))
  (is-disjoint '(not list) 'cons)
  (is-strict-subtype '(and (or (cons (not symbol)) (cons * integer)) (cons symbol))
                     '(cons * integer))
  (is-strict-subtype '(and (or (cons (not symbol)) (cons * integer)) ; 15
                       (cons * (not integer)))
                     '(cons (not symbol)))
  (is-strict-subtype '(and (or (cons symbol cons) (cons * (cons integer)))
                       (or (cons * (cons (not integer)))
                        (cons * (cons * float))))
                     '(or (cons symbol cons) (cons * (cons * float))))
  (is-unordered '(and
                  (or (cons symbol cons) (cons * (cons integer)))
                  (or (cons * (cons * (not integer))) (cons * (cons * float)))
                  (or (cons * (cons * (not float))) (cons symbol cons)))
                '(cons symbol))
  (is-strict-subtype '(cons symbol)
                     '(or (cons symbol (not integer)) (cons * integer)))
  (is-type= '(or
              (cons (eql a) (eql x))
              (cons (eql b) (eql y))
              (cons (eql c) (eql z))
              (cons (eql a) (eql y))
              (cons (eql b) (eql z))
              (cons (eql c) (eql x))
              (cons (eql a) (eql z))
              (cons (eql b) (eql x))
              (cons (eql c) (eql y)))
            '(cons (member a b c) (member x y z)))
  (is-type= '(or ; 20
              (cons (eql a) (eql x))
              (cons (eql b) (eql y))
              (cons (eql a) (eql y))
              (cons (eql b) (eql z))
              (cons (eql c) (eql x))
              (cons (eql a) (eql z))
              (cons (eql b) (eql x))
              (cons (eql c) (eql y)))
            '(and (cons (member a b c) (member x y z))
              (not (cons (eql c) (eql z)))))
  (is-type= '(cons integer single-float)
            '(or (cons fixnum single-float) (cons bignum single-float)))
  (is-type= '(cons single-float integer)
            '(or (cons single-float fixnum) (cons single-float bignum)))
  (is-strict-subtype '(cons t (cons (not long-float) symbol))
                     '(not (cons symbol (cons integer integer))))
  (is-strict-subtype '(cons (eql 3671) (cons short-float (eql -663423073525)))
                     '(not (cons t (cons (not complex) (cons integer t)))))
  (is-strict-subtype '(cons t (cons (not long-float) (integer 44745969 61634129)))
                     '(not (cons (eql -3) (cons short-float (cons t float))))) ; 25
  (is-strict-subtype '(cons integer (cons single-float cons))
                     '(cons t (cons (not complex) (not (eql 8)))))
  (is-strict-subtype '(cons (not (integer -27 30))
                       (cons rational (cons integer integer)))
                     '(not (cons integer (cons integer (eql 378132631)))))
  (is-strict-subtype '(cons (integer -1696888 -1460338)
                       (cons single-float symbol))
                     '(not (cons (not (integer -14 20))
                            (cons (not integer) cons))))
  (is-strict-subtype 'cons '(or (not (cons unsigned-byte cons))
                             (not (cons (integer -6 22) rational))))
  (is-strict-subtype '(not (cons t (cons t (cons cons t)))) ; 30
                     '(or (or (cons (cons t integer) t)
                           (not (cons t (cons t cons))))
                       (not (cons (cons (eql -27111309) t)
                             (cons t (eql 1140730))))))
  (is-strict-subtype 'cons
                     '(or (not (cons (or (cons t ratio) (cons short-float t))
                                (cons (cons (eql -7418623) (integer -9 53))
                                      (cons cons t))))
                       (not (cons (cons t (eql -265039))
                             (cons (cons t cons) t)))))
  (is-type= '(cons t (or (not (cons integer (eql 0)))
                      (not (cons (or float (eql 0)) cons))))
            'cons)
  (is-unordered 'cons
                '(or (not (cons (cons t cons) (cons t (cons unsigned-byte))))
                  (not (cons (cons integer) (cons t (cons cons))))))
  (is-strict-subtype 'cons
                     '(or (not (cons (or (eql 0) ratio) (not cons)))
                       (not (cons integer cons))))
  (is-strict-subtype '(cons t (complex (real -32 0))) ; 38
                     '(not (cons t (complex (integer * -500)))))
  (is-strict-subtype '(and (not (cons cons (cons cons))) (not (cons t cons))) t)
  (is-disjoint '(cons (eql 0) cons) '(cons unsigned-byte symbol)) ; 40
  #+(or) ; dependent on u-c-p-t
  (is-strict-subtype '(cons t (complex (real -10 -4)))
                     '(not (cons t (complex (integer -200 -100))))))

(5am:def-test cons.satisfies () ; subtypep.cons.44
  ;; We don't bother defining the functions - ctype should ignore them anyway.
  (is-unknown '(cons (not float))
              '(or (cons (satisfies foo) (satisfies bar))
                (cons (satisfies baz) (satisfies qux)))))
