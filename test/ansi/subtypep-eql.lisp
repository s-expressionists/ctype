;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:58:43 2003
;;;; Contains: Tests for subtype relationships on EQL types

(in-package #:ctype.test)

(5am:def-suite subtypep.eql :in subtypep)
(5am:in-suite subtypep.eql)

(5am:test subtypep.eql.1
  (let ((s1 (copy-seq "abc"))
        (s2 (copy-seq "abc")))
    (let ((t1 `(eql ,s1))
          (t2 `(eql ,s2)))
      (check-disjointness t1 t2 t))))

(5am:test subtypep.eql.2
  (let ((s1 (copy-seq '(a b c)))
        (s2 (copy-seq '(a b c))))
    (let ((t1 `(eql ,s1))
          (t2 `(eql ,s2)))
      (check-disjointness t1 t2 t))))

(5am:test subtypep.eql.3
  (let ((i1 (1+ most-positive-fixnum))
        (i2 (1+ most-positive-fixnum)))
    (check-equivalence `(eql ,i1) `(eql ,i2))))

(5am:test subtypep.eql.4
  (check-equivalence '(and (eql a) (eql b)) nil))

(5am:test subtypep.eql.5
  (check-all-subtypep '(eql a) '(satisfies symbolp)))

(5am:test subtypep.eql.6
  (check-disjointness '(eql 17) '(satisfies symbolp) nil))

(5am:test subtypep.eql.7
  (check-all-subtypep '(eql nil) '(satisfies symbolp)))

(5am:test subtypep.eql.8
  (check-all-not-subtypep '(satisfies symbolp) '(eql a) nil))
