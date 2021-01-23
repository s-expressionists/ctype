(in-package #:ctype)

(defclass ctype () ())
(defmethod make-load-form ((obj ctype) &optional env)
  (make-load-form-saving-slots obj :environment env))

;; A ctype corresponding to a class.
;; Note that to avoid complication, classes that could be some other ctype must
;; be instead of this. E.g. T, CONS, LIST, ARRAY, INTEGER must not end up as
;; cclass ctypes. SEQUENCE and subclasses of FUNCTION are handled specially
;; in pairwise.lisp (FUNCTION itself ends up as a CFUNCTION).
(defclass cclass (ctype)
  ((%class :initarg :class :reader cclass-class :type class)))

(defclass negation (ctype)
  ((%ctype :initarg :ctype :reader negation-ctype :type ctype)))

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
          :type (member integer ratio
                        short-float single-float double-float long-float))
   (%low :initarg :low :reader range-low :type (or real null))
   (%high :initarg :high :reader range-high :type (or real null))
   (%low-xp :initarg :lxp :reader range-low-exclusive-p :type boolean)
   (%high-xp :initarg :hxp :reader range-high-exclusive-p :type boolean)))

(defclass fpzero (ctype)
  ((%kind :initarg :kind :reader fpzero-kind
          :type (member short-float single-float double-float long-float))
   (%zero :initarg :zero :reader fpzero-zero :type float)))

(defclass ccomplex (ctype)
  (;; The upgraded complex part type is some thing that can be meaningfully
   ;; compared with EQUAL. CL:* is always allowed and has the standard meaning.
   (%ucpt :initarg :ucpt :reader ccomplex-ucpt)))

(defclass cmember (ctype)
  ((%members :initarg :members :reader cmember-members :type list)))

(defclass carray (ctype)
  ((%simplicity :initarg :simplicity :reader carray-simplicity
                :type (member :simple :complex))
   ;; Can be * to indicate all possible. Should be more efficient that way.
   ;; Other than that, it's just something equal-comparable. Also should
   ;; be what's returned by array-element-type.
   (%uaet :initarg :uaet :reader carray-uaet)
   ;; Either a list of dimensions (which are either positive integers or *)
   ;; or * indicating nothing specified.
   (%dims :initarg :dims :reader carray-dims :type (or list (eql *)))))

(defclass charset (ctype)
  ((%pairs :initarg :pairs :reader charset-pairs
           ;; A list of (char-code . char-code) pairs, each representing
           ;; an inclusive interval of codepoints, in order and exclusive.
           :type list)))

(defclass cvalues (ctype)
  ((%required :initarg :required :reader cvalues-required
              ;; A proper list of ctypes.
              :type list)
   (%optional :initarg :optional :reader cvalues-optional
              ;; A proper list of ctypes.
              :type list)
   (%rest :initarg :rest :reader cvalues-rest :type ctype)))

(defclass lambda-list (ctype)
  ((%required :initarg :required :reader lambda-list-required
              ;; A proper list of ctypes.
              :type list)
   (%optional :initarg :optional :reader lambda-list-optional
              ;; A proper list of ctypes.
              :type list)
   (%rest :initarg :rest :reader lambda-list-rest
          ;; Having no &rest is indicated by setting this to (bot).
          :type ctype)
   ;; Is there a &key? This may be true even if no actual ctypes are
   ;; specified for keyword parameters.
   (%keyp :initarg :keyp :reader lambda-list-keyp :type boolean)
   (%keys :initarg :keys :reader lambda-list-key
          ;; A proper list of (keyword . ctype) pairs.
          :type list)
   ;; Is there &allow-other-keys?
   (%aokp :initarg :aokp :reader lambda-list-aokp :type boolean)))

(defclass cfunction (ctype)
  (;; A specification of * is equivalent to (&rest t).
   (%lambda-list :initarg :lambda-list :reader cfunction-lambda-list
                 :type lambda-list)
   ;; A specification of * is equivalent to (values &rest t).
   (%returns :initarg :returns :reader cfunction-returns :type cvalues)))

(defclass csatisfies (ctype)
  ((%fname :initarg :fname :reader csatisfies-fname)))
