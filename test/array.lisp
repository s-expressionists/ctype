;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  1 16:23:57 2003
;;;; Contains: Tests for subtype relationships on array types
;;;; Adapted from ANSI tests' subtypep-array.lsp.

(in-package #:ctype.test)

(5am:def-suite array :in ctype)
(5am:in-suite array)

;; from ansi-aux.lsp
(defparameter *array-element-types*
  '(t (integer 0 0)
    bit (unsigned-byte 8) (unsigned-byte 16)
    (unsigned-byte 32) float short-float
    single-float double-float long-float
    nil character base-char symbol boolean null))

;;; Originally I tried writing these as macrolets, so they'd expand into
;;; a bunch of distinct is-type= whatever forms, but that was too clever
;;; and also got SBCL to drop into LDB somehow.
(defmacro do-aets ((aet &optional (starp t)) &body body)
  `(dolist (,aet ,(if starp
                      '(cons '* *array-element-types*)
                      '*array-element-types*))
     ,@body))

(defmacro do-ranks ((rank) &body body)
  ;; ctype should work regardless of the host's array-rank-limit,
  ;; so unlike ANSI tests we ignore it and always use 16.
  ;; array-rank-limit is only required to be at least 8.
  `(dotimes (,rank 16) ,@body))

(5am:def-test array.id () ; subtypep.array.1-2
  (are-type= array (array) (array *) (array * *))
  (are-type= vector (vector *))
  (do-aets (aet nil)
    (is-strict-subtype `(array ,aet) 'array)
    (is-type= `(array ,aet) `(array ,aet *))
    (is-type= `(vector ,aet) `(array ,aet 1))
    (is-type= `(vector ,aet) `(array ,aet (*)))
    (is-type= `(simple-array ,aet) `(simple-array ,aet *))
    (is-type= `(simple-array ,aet 1) `(simple-array ,aet (*))))
  (is-type= 'simple-vector '(simple-array t 1))
  (is-type= 'bit-vector '(vector bit))
  (is-type= 'simple-bit-vector '(simple-array bit 1))
  (is-type= 'base-string '(vector base-char))
  (is-type= 'simple-base-string '(simple-array base-char 1)))

(defun dims (n &optional (dim '*))
  (make-list n :initial-element dim))

(5am:def-test array.rank () ; subtypep.array.3-4
  ;; testing every AET is kind of overkill
  (dolist (aet '(* t bit base-char))
    (do-ranks (rank)
      (is-strict-subtype `(array ,aet ,rank) 'array)
      (is-strict-subtype `(array ,aet ,rank) `(array ,aet))
      (is-strict-subtype `(array ,aet ,rank) `(array ,aet *))
      (is-type= `(array ,aet ,rank) `(array ,aet ,(dims rank))))))

(5am:def-test array.rank.subtype () ; subtypep.array.5
  (dolist (aet '(* t bit base-char))
    (is-type= `(array ,aet ()) `(array ,aet 0))
    (do-ranks (rank)
      (when (> rank 0)
        (is-strict-subtype `(array ,aet ,(dims rank 0))
                           `(array ,aet ,rank))
        (is-strict-subtype `(array ,aet ,(dims rank 1))
                           `(array ,aet ,rank)))
      (loop for j below (min 4 rank)
            for d1 = `(,@(dims j) ,@(dims (- rank j) 2))
            for a1 = `(array ,aet ,d1)
            for d2 = `(,@(dims j 2) ,@(dims (- rank j)))
            for a2 = `(array ,aet ,d2)
            when (> j 0)
              do (is-strict-subtype a1 `(array ,aet ,rank))
                 (is-strict-subtype a2 `(array ,aet ,rank))))))

(5am:def-test array.conjoin.dims () ; subtypep.array.6
  (do-aets (aet)
    (is-type= `(and (array ,aet (* 10 * * *)) (array ,aet (* * * 29 *)))
              `(array ,aet (* 10 * 29 *)))))

(5am:def-test array.disjoint.uaets () ; subtypep.array.7
  (loop for aet1 in *array-element-types*
        for uaet1 = (upgraded-array-element-type aet1)
        for t1 = (specifier-ctype uaet1)
        nconc (loop for aet2 in *array-element-types*
                    for uaet2 = (upgraded-array-element-type aet2)
                    for t2 = (specifier-ctype uaet2)
                    if (ctype= t1 t2)
                      do (is-type= `(array ,aet1) `(array ,aet2))
                    else
                      ;; this assumes no NIL NIL from ctype=,
                      ;; but we should expect that from what's in there anyway.
                      do (is-disjoint `(array ,aet1) `(array ,aet2)))))

(5am:def-test array.disjoint.ranks () ; subtypep.array.8-9
  (do-ranks (i)
    (dotimes (j i)
      (is-disjoint `(array t ,(dims i 1)) `(array t ,(dims j 1)))
      (is-disjoint `(array t ,i) `(array t ,j)))))

;;; TODO: string-is-not-vector-of-character etc, depend on client
(5am:def-test string-subtype ()
  (is-subtype '(vector character) 'string)
  (is-subtype '(vector base-char) 'string)
  (is-subtype '(vector standard-char) 'string)
  (is-subtype '(vector extended-char) 'string)
  (is-subtype '(simple-array character (*)) 'simple-string)
  (is-subtype '(simple-array base-char (*)) 'simple-string)
  (is-subtype '(simple-array standard-char (*)) 'simple-string)
  (is-subtype '(simple-array extended-char (*)) 'simple-string))
