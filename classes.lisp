(in-package #:ctype)

(defclass ctype () ())
(defmethod make-load-form ((obj ctype) &optional env)
  (make-load-form-saving-slots obj :environment env))

;; A ctype corresponding to a class.
;; Note that to avoid complication, classes that could be some other ctype must
;; be instead of this. E.g. T, CONS, LIST, ARRAY, INTEGER must not end up as
;; cclass ctypes.
(defclass cclass (ctype)
  ((%class :initarg :class :reader cclass-class :type class)))

(defclass negation (ctype)
  ((%ctype :initarg :ctype :reader negation-ctype :type ctype)))
(defun negation (ctype) (make-instance 'negation :ctype ctype))

(defclass junction (ctype)
  ((%ctypes :initarg :ctypes :reader junction-ctypes
            :type list)))

(defclass conjunction (junction) ())
(defclass disjunction (junction) ())

(defclass ccons (ctype)
  ((%car :initarg :car :reader ccons-car :type ctype)
   (%cdr :initarg :cdr :reader ccons-cdr :type ctype)))

(defclass range (ctype)
  ((%kind :initarg :kind :reader range-kind
          :type (or integer ratio single-float double-float))
   (%low :initarg :low :reader range-low :type (or real null))
   (%high :initarg :high :reader range-high :type (or real null))
   (%low-xp :initarg :lxp :reader range-low-exclusive-p :type boolean)
   (%high-xp :initarg :hxp :reader range-high-exclusive-p :type boolean)))

(defclass ccomplex (ctype)
  (;; The upgraded complex part type is some thing that can be meaningfully
   ;; compared with EQUAL. CL:* is always allowed and has the standard meaning.
   (%ucpt :initarg :ucpt :reader ccomplex-ucpt)))

(defclass cmember (ctype)
  ((%members :initarg :members :reader cmember-members :type list)))

(defclass carray (ctype)
  (;; Can be * to indicate all possible. Should be more efficient that way.
   (%uaet :initarg :uaet :reader carray-uaet)
   (%dims :initarg :dims :reader carray-dims)))

(defclass csatisfies (ctype)
  ((%fname :initarg :fname :reader csatisfies-fname)))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSING
;;;

(defun specifier-ctype (type-specifier &optional env)
  (let ((ts (cleavir-env:type-expand env type-specifier)))
    (etypecase ts
      (class
       (case (class-name class)
         ((null) (cmember nil))
         ((cons) (ccons (top) (top)))
|#
