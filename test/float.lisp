;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:55:37 2003
;;;; Contains: Tests for subtype relationships on float types
;;;; Adapted from ANSI tests' subtypep-float.lsp.

(in-package #:ctype.test)

(5am:def-suite float :in ctype)
(5am:in-suite float)

(5am:def-test float.subtypep.format () ; subtypep.float.1
  (is-subtype 'short-float 'float)
  (is-subtype 'single-float 'float)
  (is-subtype 'double-float 'float)
  (is-subtype 'long-float 'float))

(5am:def-test float.disjoint.format () ; subtypep.float.3-7
  ;; Check if float types are disjoint or identical as defined
  ;; in the CLHS page on SHORT-FLOAT etc.
  ;; The various 5am:pass are so that the number of tests is
  ;; not affected by the client.
  ;; FIXME: Better reasons.
  ;; FIXME: Could test ctype=/disjointp more exhaustively
  ;; (checking that the intersection is NIL, that kinda thing)
  (let ((short (specifier-ctype 'short-float))
        (single (specifier-ctype 'single-float))
        (double (specifier-ctype 'double-float))
        (long (specifier-ctype 'long-float)))
    (cond ((ctype= short single)
           (5am:pass)
           (cond ((ctype= single double)
                  (5am:pass)
                  (if (ctype= double long)
                      (5am:pass) ; 1 format
                      ;; 2 formats: short/single/double, long
                      (5am:is (disjointp double long))))
                 (t
                  (5am:is (disjointp single double))
                  (if (ctype= double long)
                      ;; 2 formats: short/single, double/long
                      (5am:pass)
                      ;; 3 formats: short/single, double, long
                      (5am:is (disjointp double long))))))
          (t
           (5am:is (disjointp short single))
           (cond ((ctype= single double)
                  (5am:pass)
                  ;; 2 formats: short, single/double/long
                  ;; if double /= long, we have 3, short, single/double, long
                  ;; which is not allowed by the standard.
                  (5am:is (ctype= double long)))
                 (t
                  (5am:is (disjointp single double))
                  (if (ctype= double long)
                      (5am:pass) ; 3: short, single, double/long
                      ;; 4 formats, all distinct
                      (5am:is (disjointp double long)))))))))

(5am:def-test float.ranges () ; 8-23
  (is-strict-subtype '(short-float 0.0s0 10.0s0) '(short-float 0.0s0 11.0s0))
  (is-strict-subtype '(single-float 0.0f0 10.0f0) '(single-float 0.0f0 11.0f0))
  (is-strict-subtype '(double-float 0.0d0 10.0d0) '(double-float 0.0d0 11.0d0))
  (is-strict-subtype '(long-float 0.0l0 10.0l0) '(long-float 0.0l0 11.0l0))
  (is-strict-subtype '(short-float 0.0s0 (10.0s0)) '(short-float 0.0s0 10.0s0))
  (is-strict-subtype '(single-float 0.0f0 (10.0f0)) '(single-float 0.0f0 10.0f0))
  (is-strict-subtype '(double-float 0.0d0 (10.0d0)) '(double-float 0.0d0 10.0d0))
  (is-strict-subtype '(long-float 0.0l0 (10.0l0)) '(long-float 0.0l0 10.0l0)))

(5am:def-test float.conjoin () ; 24-27
  (is-type= '(and (short-float 0.0s0 2.0s0) (short-float 1.0s0 3.0s0))
            '(short-float 1.0s0 2.0s0))
  (is-type= '(and (single-float 0.0f0 2.0f0) (single-float 1.0f0 3.0f0))
            '(single-float 1.0f0 2.0f0))
  (is-type= '(and (double-float 0.0d0 2.0d0) (double-float 1.0d0 3.0d0))
            '(double-float 1.0d0 2.0d0))
  (is-type= '(and (long-float 0.0l0 2.0l0) (long-float 1.0l0 3.0l0))
            '(long-float 1.0l0 2.0l0)))

(5am:def-test float.disjoin () ; original
  (loop for format in '(short-float single-float double-float long-float)
        for zero = (coerce 0 format)
        for one = (coerce 1 format)
        for two = (coerce 2 format)
        for three = (coerce 3 format)
        do (is-type= `(or (,format ,zero ,two) (,format ,two ,three))
                     `(,format ,zero ,three))
           (is-type= `(or (,format ,zero (,two)) (,format ,two ,three))
                     `(,format ,zero ,three))
           (is-type= `(or (,format ,zero ,two) (,format ,one ,three))
                     `(,format ,zero ,three))
           (is-type= `(or (,format * ,three) (,format ,three)) format)
           (is-strict-subtype `(or (,format ,zero ,one) (,format ,two ,three))
                              `(,format ,zero ,three))
           (is-strict-subtype `(or (,format ,zero (,two)) (,format (,two) ,three))
                              `(,format ,zero ,three))))

;;; Signed zero tests

(5am:def-test float.zero () ; subtypep.[format].zero.1-10
  (loop for format in '(short-float single-float double-float long-float)
        for nzero = (coerce -0.0 format)
        for pzero = (coerce 0.0 format)
        do (is-type= `(,format ,pzero)
                     `(or (,format (,pzero)) (member ,nzero ,pzero)))
           (is-type= `(,format ,nzero) `(,format ,pzero))
           (is-type= `(,format * ,nzero) `(,format * ,pzero))
           (is-type= `(,format (,nzero)) `(,format (,pzero)))
           (is-type= `(,format * (,nzero)) `(,format * (,pzero)))
        if (distinct-zeroes-p format)
          do (is-strict-subtype `(or (,format (,pzero)) (member ,pzero))
                                `(,format ,pzero))
             (is-strict-subtype `(or (,format (,pzero)) (member ,nzero))
                                `(,format ,pzero))
        else
          do (is-type= `(or (,format (,pzero)) (member ,pzero))
                       `(,format ,pzero))
             (is-type= `(or (,format (,pzero)) (member ,nzero))
                       `(,format ,pzero))))
