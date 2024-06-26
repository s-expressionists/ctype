;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:58:06 2003
;;;; Contains: Tests for subtype relationships on member types
;;;; Adapted from ANSI tests' subtypep-member.lsp and subtypep-eql.lsp.

(in-package #:ctype.test)

(5am:def-suite member :in ctype)
(5am:in-suite member)

(5am:def-test eql.sequences () ; subtypep.eql.1-2
  (is-disjoint `(eql ,(copy-seq "abc")) `(eql ,(copy-seq "abc")))
  (is-disjoint `(eql ,(copy-seq '(a b c))) `(eql ,(copy-seq '(a b c)))))

(5am:def-test eql.bignum () ; subtypep.eql.3
  (is-type= `(eql ,(1+ (most-positive-fixnum)))
            `(eql ,(1+ (most-positive-fixnum)))))

(5am:def-test eql.disjoint () ; subtypep.eql.4
  (is-disjoint '(eql a) '(eql b)))

(5am:def-test eql.satisfies () ; subtypep.eql.5-8
  (is-subtype '(eql a) '(satisfies symbolp))
  (is-maybe-not-subtype '(satisfies symbolp) '(eql a))
  (is-disjoint '(eql 17) '(satisfies symbolp))
  (is-subtype '(eql nil) '(satisfies symbolp)))

(5am:def-test member.empty () ; subtypep.member.3
  (is-type= '(member) 'nil))

(5am:def-test member.symbol () ; subtypep.member.1-2, 4-8, 10, 20-43
  (is-strict-subtype '(member a b c) '(member a b c d))
  (is-strict-subtype '(eql a) '(member a b c))
  (is-strict-subtype '(member a b c d e) 'symbol) ; 4
  (is-unordered '(member a b 10 d e) 'symbol)
  (is-strict-subtype 'null '(member a b nil c d e))
  (is-unordered 'null '(member a b c d e))
  (is-strict-subtype '(member :a :b :c) 'keyword) ; 10
  (is-type= '(and (member a b c d) (member e d b f g)) '(member b d)) ; 20
  (is-type= '(and (member a b c d) (member e d f g)) '(eql d))
  (is-type= '(and (member a b c d) (member e f g)) nil)
  (is-type= '(or (member a b c) (member z b w)) '(member z a b w c))
  (is-type= '(or (member a b c) (eql d)) '(member d c b a))
  (is-type= 'boolean '(member t nil)) ; 25
  (is-type= '(or (eql a) (eql b)) '(member a b))
  (is-subtype '(member a b c d) '(satisfies symbolp))
  (is-strict-subtype '(member a b c d) 't)
  (is-unordered '(member a b 10 z) '(satisfies symbolp))
  (is-disjoint '(member 1 6 10) '(satisfies symbolp)) ; 30
  (is-type= '(member a b c d) '(member c d b a))
  (is-unordered '(not (member a b 10 z)) '(satisfies symbolp))
  (is-unordered '(satisfies symbolp) '(member a b 10 z))
  (is-unordered '(member a b 10 z) '(not (satisfies symbolp)))
  (is-maybe-not-subtype '(satisfies symbolp) '(member a b c d)) ; 35
  (is-disjoint '(eql a) '(or (member b c d) (eql e)))
  (is-type= '(and (member a b c d) (not (eql c))) '(member a b d))
  (is-type= '(and (member a b c d e f g) (not (member b f)))
            '(member a c d e g))
  (is-type= '(and (not (member b d e f g)) (not (member x y b z d)))
            '(not (member b d e f g x y z)))
  (is-type= '(and (not (eql a)) (not (eql b))) '(not (member a b))) ; 40
  (is-type= '(and (not (eql a)) (not (eql b)) (not (eql c)))
            '(not (member c b a)))
  (is-type= '(and (not (member a b)) (not (member b c)))
            '(not (member c b a)))
  (is-type= '(and (not (member a g b k e)) (not (member b h k c f)))
            '(not (member c b k a e f g h))))

(5am:def-test member.integer () ; subtypep.member.9, 13-16, 19, 44
  (let ((b1 (1+ (most-positive-fixnum))) (b2 (1+ (most-positive-fixnum))))
    (is-type= `(member 10 ,b1 20) `(member 10 20 ,b2))
    (is-type= `(member 0 ,b1) `(member 0 ,b2)))
  (is-strict-subtype '(member 10 20 30) '(integer 0 100))
  (is-strict-subtype '(integer 3 6) '(member 0 1 2 3 4 5 6 7 8 100))
  (is-unordered '(integer 3 6) '(member 0 1 2 3 5 6 7 8))
  (is-type= '(integer 2 5) '(member 2 5 4 3))
  (is-type= '(and (integer 0 30) (not (member 3 4 5 9 10 11 17 18 19)))
            '(or (integer 0 2) (integer 6 8) (integer 12 16) (integer 20 30))))

(5am:def-test member.sequence () ; subtypep.member.11-12, 17-18
  (let ((b1 (copy-list '(a))) (b2 (copy-list '(a))))
    (is-unordered `(member 10 ,b1 20) `(member 10 20 ,b2))
    (is-type= `(member 10 ,b1 20) `(member 10 20 ,b1)))
  (let ((s1 (copy-seq "abc")) (s2 (copy-seq "abc")))
    (is-disjoint `(member ,s1) `(member ,s2)))
  (let ((s1 (copy-seq '(a b c))) (s2 (copy-seq '(a b c))))
    (is-disjoint `(member ,s1) `(member ,s2))))
