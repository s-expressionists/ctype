(in-package #:ctype)

;;; Make conjunctions and disjunctions.
(defun conjunction (&rest ctypes)
  (cond ((null ctypes) (top))
        ((null (rest ctypes)) (first ctypes))
        (t (make-instance 'conjunction :ctypes ctypes))))
(defun disjunction (&rest ctypes)
  (cond ((null ctypes) (bot))
        ((null (rest ctypes)) (first ctypes))
        (t (make-instance 'disjunction :ctypes ctypes))))

;;; top and bottom types are special cases, so we give them particular
;;; identities. Normalized top ctypes must be identical to (top) and
;;; normalized bottom ctypes must be identical to (bot).

(defvar *top* (make-instance 'conjunction :ctypes nil) #+(or)(conjunction nil))
(defvar *bot* (make-instance 'disjunction :ctypes nil) #+(or)(disjunction nil))

(defun top () *top*)
(defun top-p (ctype) (eq ctype *top*))

(defun bot () *bot*)
(defun bot-p (ctype) (eq ctype *bot*))

;;; Others
(defun negation (ctype) (make-instance 'negation :ctype ctype))

(defun cclass (class) (make-instance 'cclass :class class))

(defun ccons (car cdr)
  (if (or (bot-p car) (bot-p cdr))
      (bot)
      (make-instance 'ccons :car car :cdr cdr)))

(defun range (kind low lxp high hxp)
  (multiple-value-bind (low lxp high hxp)
      (if (eq kind 'integer)
          (values (if (and low lxp) (1+ low) low) nil
                  (if (and high hxp) (1- high) high) nil)
          (values low lxp high hxp))
    (if (and low high (or (> low high) (and (= low high) (or lxp hxp))))
        (bot)
        (make-instance 'range
          :kind kind :low low :lxp lxp :high high :hxp hxp))))

(defun fpzero (kind zero) (make-instance 'fpzero :kind kind :zero zero))

(defun charset (pairs)
  (if (null pairs)
      (bot)
      (make-instance 'charset :pairs pairs)))

(defun carray (simplicity uaet eaet dims)
  (make-instance 'carray
    :simplicity simplicity :uaet uaet :eaet eaet :dims dims))

(defun ccomplex (ucpt) (make-instance 'ccomplex :ucpt ucpt))

(defun cmember (&rest members)
  (if members
      (make-instance 'cmember :members members)
      (bot)))

(defun cvalues (required optional rest)
  (make-instance 'cvalues
    :required required :optional optional :rest rest))

(defun values-top () (cvalues nil nil (top)))
;; Note that this is NOT (values &rest nil), the type of (values).
;; This type indicates no values are possible, as from an error.
;; In the future, there may be a distinguished representation for this.
(defun values-bot () (cvalues (list (bot)) nil (bot)))

(defun single-value (ctype)
  (assert (not (cvalues-p ctype)))
  (cvalues (list ctype) nil (bot)))

(defun csatisfies (fname)
  (make-instance 'csatisfies :fname fname))

(defun cfunction (lambda-list returns)
  (make-instance 'cfunction :lambda-list lambda-list :returns returns))

(defun lambda-list-top ()
  (make-instance 'lambda-list
    :required nil :optional nil :rest (top)
    :keyp nil :keys nil :aokp nil))

(defun function-top ()
  (cfunction (lambda-list-top) (values-top)))
