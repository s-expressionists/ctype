(in-package #:ctype)

;;; Complex constants - this is a cheap version of alexandria:define-constant

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reconstant (name value test)
    (if (not (boundp name))
        value
        (let ((old (symbol-value name)))
          (if (funcall test value old)
              old
              (error "Cannot redefine constant ~a to ~a" name value))))))

(defmacro define-constant (name value &key (test ''eql))
  `(defconstant ,name (reconstant ',name ,value ,test)))

;;; Template for implementation-specific-code
#+(or)
(progn

;; Is OBJECT a ratio?
(defun ratiop (object) ...) ; determine whether OBJECT is a ratio

;; An alist from existing float subtypes to the names of predicates for them
(define-constant +floats+ '(...) :test #'equal)

#|
+standard-charset+ and +base-charset+ are lists of conses
and each cons represents a range of character codes.
  ((10 . 10) (32 . 126)) means that character codes 10, 32, 33, ... 125, 126
are the standard characters.

Here is a helper function that can be used to determine these ranges:

(defun find-ranges (predicate start end)
  (loop :with ranges := nil
        :with inside-range-p := nil
        :with range-start := nil
        :with range-end := nil
        :for i :from start :to end
        :do (if (funcall predicate (code-char i))
                (if inside-range-p
                    (incf range-end)
                    (setq range-start i
                          range-end   i
                          inside-range-p t))
                (when inside-range-p
                  (progn
                    (push (cons range-start range-end) ranges)
                    (setq inside-range-p nil))))
        :finally (return (nreverse ranges))))

Discover +standard-charset+ via: (assuming there are less than 2^20 char codes)

(find-ranges #'standard-char-p 0 (expt 2 20))

and base-charset via
(find-ranges (lambda (x) (typep x 'base-char)) 0 (expt 2 20))
|#

;; Charset for standard-char
;; If the implementation uses a superset of ASCII,
;; this will be '((10 . 10) (32 . 126))
(define-constant +standard-charset+ '(...) :test #'equal)
;; Charset for base-char
(define-constant +base-charset+ '(...) :test #'equal)

;; A list of upgraded array element types for arrays that are strings
(define-constant +string-uaets+ '(...) :test #'equal)

;;; This should be true unless (and array (not simple-array)) = NIL.
;;; This is used only in the parser - if you make array ctypes directly be sure
;;; to always apply simplicity :simple, if complex arrays do not exist.
;;; FIXME?: Right now there's no provision for partial existence of complex
;;; arrays - for example if they only exist for vectors.
(define-constant +complex-arrays-exist-p+ ...)

;;; List of (classname type-specifier); specifier-ctype will resolve
;;; classes with the former name in the same way as it would resolve the
;;; specifier. CL names (e.g. FIXNUM, SIMPLE-BIT-VECTOR) are already handled
;;; and don't need to be specified here.
(define-constant +class-aliases+ '(...) :test #'equal)

;;; Function that determines of SUB is a subclass of SUPERCLASS.
;;; Only classes are given as arguments (i.e. not names)
(defun subclassp (sub super) ...)

;;; Function that resolves macro types (i.e. those defined with DEFTYPE) at top
;;; level. Analogously to MACROEXPAND, should return the expansion as the first
;;; value and a boolean indicating whether any expansion was done as the second.
(defun typexpand (type-specifier environment) ...)

;;; Macro defined such that
;;; (typep object '(complex foo)) is equivalent to
;;; (complex-ucptp object ufoo), where ufoo is (upgraded-complex-part-type 'foo)
(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     ...))

) ; #+(or)(progn ...)
