(in-package #:ctype.test)

;;; entry points
(defun run (*client*) (5am:run 'ctype))

(defun run! (*client*) (5am:run! 'ctype))

;;; misc operators
;; like ansi-test's check-subtypep, but more 5am-y
(defun is-subtypep (type1 type2 &optional (is-sub t) (should-be-valid is-sub))
  (multiple-value-bind (sub valid) (subtypep type1 type2)
    (5am:is-true (if valid (eq is-sub sub) (not should-be-valid))
                 "~s was~:[ not~;~] recognizably~:[ not~;~] a subtype of ~s"
                 type1 valid is-sub type2)))

(defun is-disjointp (type1 type2 &optional (is-dis t) (should-be-valid is-dis))
  (multiple-value-bind (dis valid)
      (ctype:disjointp ctype-extrinsic:*client*
                       (specifier-ctype type1) (specifier-ctype type2))
    (5am:is-true (if valid (eq is-dis dis) (not should-be-valid))
                 "~s was~:[ not~;~] recognizably~:[ not~;~] disjoint from ~s"
                 type1 valid is-dis type2)))

(defun is-type= (type1 type2 &optional (is-eqv t) (should-be-valid is-eqv))
  (multiple-value-bind (eqv valid)
      (ctype:ctype= ctype-extrinsic:*client*
                    (specifier-ctype type1) (specifier-ctype type2))
    (5am:is-true (if valid (eq is-eqv eqv) (not should-be-valid))
                 "~s was~:[ not~;~] recognizably~:[ not~;~] equivalent to ~s"
                 type1 valid is-eqv type2)))

;;; ansi-tests compatibility
(defmacro deftest (name form &rest expected)
  `(5am:test ,name
             (5am:is (equal '(,@expected)
                            (multiple-value-list ,form)))))

(defmacro signals-error (form condition-type)
  `(5am:signals ,condition-type ,form))

;; the idea of this is that it signals errors even on safety 0
;; but we're not integrated with the compiler anyway.
(defmacro signals-error-always (form condition-type)
  `(signals-error ,form ,condition-type))
