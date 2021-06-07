(in-package #:ctype)

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

;;;

(declaim (inline ratiop))
(defun ratiop (object)
  #+clasp (ext::ratiop object)
  #+sbcl (sb-int:ratiop object)
  #+ccl (ccl:ratiop object)
  #+sicl (typep object 'ratio)
  #-(or clasp sbcl ccl sicl) (error "RATIOP not defined for implementation"))

(define-constant +floats+
  #+clasp '((single-float . core:single-float-p)
            (double-float . core:double-float-p))
  #+sbcl '((single-float . sb-int:single-float-p)
           (double-float . sb-int:double-float-p))
  ;; SHORT-FLOAT is a SINGLE-FLOAT on CCL
  ;; and (apropos "float-p" :ccl) doesn't return single-float-p :
  ;;   https://ccl.clozure.com/manual/chapter4.11.html#floating-point
  #+ccl '((single-float . ccl::short-float-p)
          (double-float . ccl::double-float-p))
  #+sicl '((single-float . sicl-arithmetic:short-float-p)
           (double-float . sicl-arithmetic:double-float-p))
  #-(or clasp sbcl ccl sicl) (error "FLOATS not defined for implementation")
  :test #'equal)

#|

Below +standard-charset+ and +base-charset+ are a list of conses
and each cons represents a range of character codes.
  ((10 . 10) (32 . 126)) means that character codes 10, 32, 33, ... 125, 126
are the standard characters.

(defun find-ranges (predicate start end)
  (loop :with ranges := nil
        :with inside-range-p := nil
        :with range-start := nil
        :with range-end := nil
        :for i :from start :to end
        :do (if (funcall predicate i)
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

Discover +base-charset+ via:

(find-ranges (lambda (x) (typep (code-char x) 'standard-char))
             0 (expt 2 20))

|#

(define-constant +standard-charset+
  ;; In ASCII (or Unicode)
  #+(or clasp sbcl ccl sicl) '((10 . 10) (32 . 126))
  #-(or clasp sbcl ccl sicl) (error "STANDARD-CHARSET not defined for implementation")
  :test #'equal)

#| Discover +base-charset+ via:

(find-ranges (lambda (x) (typep (code-char x) 'base-char))
             0 (expt 2 20))

|#

(define-constant +base-charset+
  #+clasp '((0 . 255))
  #+sbcl '((0 . 127))
  #+ccl '((0 . 55295))
  #+sicl '((0 . #x10FFFF))
  #-(or clasp sbcl ccl sicl) (error "BASE-CHARSET not defind for implementation")
  :test #'equal)

(define-constant +string-uaets+ ; Upgraded Array Element Type
  #+clasp '(base-char character)
  #+sbcl '(nil base-char character)
  #+ccl '(nil base-char)
  #+sicl '(character)
  #-(or clasp sbcl ccl sicl) (error "STRING-UAETS not defined for implementation")
  :test #'equal)

;;; This should be T unless (and array (not simple-array)) = NIL.
;;; This is used only in the parser - if you make array ctypes directly be sure
;;; to always apply simplicity :simple, if complex arrays do not exist.
;;; FIXME?: Right now there's no provision for partial existence of complex
;;; arrays - for example if they only exist for vectors.
(define-constant +complex-arrays-exist-p+
  #+clasp t
  #+sbcl t
  #+ccl t
  #+sicl nil
  #-(or clasp sbcl ccl sicl)
  (error "COMPLEX-ARRAYS-EXIST-P not defined for implementation"))

(declaim (inline simple-array-p))
(defun simple-array-p (object)
  #+clasp (if (cleavir-primop:typeq object core:abstract-simple-vector) t nil)
  #+sbcl (sb-kernel:simple-array-p object)
  #+ccl (ccl::simple-array-p object)
  #-(or clasp sbcl ccl)
  (if +complex-arrays-exist-p+
      (error "SIMPLE-ARRAY-P not defined for implementation")
      t))

;;; List of (classname type-specifier); specifier-ctype will resolve
;;; classes with the former name in the same way as it would resolve the
;;; specifier. CL names (e.g. FIXNUM, SIMPLE-BIT-VECTOR) are already handled
;;; and don't need to be specified here.
(define-constant +class-aliases+
  #+clasp '((core:abstract-simple-vector (simple-array * (*)))
            (core:simple-vector-fixnum (simple-array fixnum (*)))
            (core:simple-vector-byte2-t (simple-array ext:byte2 (*)))
            (core:simple-vector-byte4-t (simple-array ext:byte4 (*)))
            (core:simple-vector-byte8-t (simple-array ext:byte8 (*)))
            (core:simple-vector-byte16-t (simple-array ext:byte16 (*)))
            (core:simple-vector-byte32-t (simple-array ext:byte32 (*)))
            (core:simple-vector-byte64-t (simple-array ext:byte64 (*)))
            (core:simple-vector-int2-t (simple-array ext:integer2 (*)))
            (core:simple-vector-int4-t (simple-array ext:integer4 (*)))
            (core:simple-vector-int8-t (simple-array ext:integer8 (*)))
            (core:simple-vector-int16-t (simple-array ext:integer16 (*)))
            (core:simple-vector-int32-t (simple-array ext:integer32 (*)))
            (core:simple-vector-int64-t (simple-array ext:integer64 (*)))
            (core:simple-vector-float (simple-array single-float (*)))
            (core:simple-vector-double (simple-array double-float (*)))
            (core:simple-character-string (simple-array character (*)))

            (core:complex-vector (and (not simple-array) (array * (*))))
            (core:bit-vector-ns (and (not simple-array) (array bit (*))))
            (core:complex-vector-fixnum
             (and (not simple-array) (array fixnum (*))))
            (core:complex-vector-byte2-t
             (and (not simple-array) (array ext:byte2 (*))))
            (core:complex-vector-byte4-t
             (and (not simple-array) (array ext:byte4 (*))))
            (core:complex-vector-byte8-t
             (and (not simple-array) (array ext:byte8 (*))))
            (core:complex-vector-byte16-t
             (and (not simple-array) (array ext:byte16 (*))))
            (core:complex-vector-byte32-t
             (and (not simple-array) (array ext:byte32 (*))))
            (core:complex-vector-byte64-t
             (and (not simple-array) (array ext:byte64 (*))))
            (core:complex-vector-int2-t
             (and (not simple-array) (array ext:integer2 (*))))
            (core:complex-vector-int4-t
             (and (not simple-array) (array ext:integer4 (*))))
            (core:complex-vector-int8-t
             (and (not simple-array) (array ext:integer8 (*))))
            (core:complex-vector-int16-t
             (and (not simple-array) (array ext:integer16 (*))))
            (core:complex-vector-int32-t
             (and (not simple-array) (array ext:integer32 (*))))
            (core:complex-vector-int64-t
             (and (not simple-array) (array ext:integer64 (*))))
            (core:complex-vector-float
             (and (not simple-array) (array single-float (*))))
            (core:complex-vector-double
             (and (not simple-array) (array double-float (*))))
            (core:str8ns (and (not simple-array) (array base-char (*))))
            (core:str-wns (and (not simple-array) (array character (*))))
            (core:complex-vector-t (and (not simple-array) (array t (*))))

            (core:simple-mdarray (and (not vector) simple-array))
            (core:simple-mdarray-bit (and (not vector) (simple-array bit)))
            (core:simple-mdarray-fixnum
             (and (not vector) (simple-array fixnum)))
            (core:simple-mdarray-byte2-t
             (and (not vector) (simple-array ext:byte2)))
            (core:simple-mdarray-byte4-t
             (and (not vector) (simple-array ext:byte4)))
            (core:simple-mdarray-byte8-t
             (and (not vector) (simple-array ext:byte8)))
            (core:simple-mdarray-byte16-t
             (and (not vector) (simple-array ext:byte16)))
            (core:simple-mdarray-byte32-t
             (and (not vector) (simple-array ext:byte32)))
            (core:simple-mdarray-byte64-t
             (and (not vector) (simple-array ext:byte64)))
            (core:simple-mdarray-int2-t
             (and (not vector) (simple-array ext:integer2)))
            (core:simple-mdarray-int4-t
             (and (not vector) (simple-array ext:integer4)))
            (core:simple-mdarray-int8-t
             (and (not vector) (simple-array ext:integer8)))
            (core:simple-mdarray-int16-t
             (and (not vector) (simple-array ext:integer16)))
            (core:simple-mdarray-int32-t
             (and (not vector) (simple-array ext:integer32)))
            (core:simple-mdarray-int64-t
             (and (not vector) (simple-array ext:integer64)))
            (core:simple-mdarray-float
             (and (not vector) (simple-array single-float)))
            (core:simple-mdarray-double
             (and (not vector) (simple-array double-float)))
            (core:simple-mdarray-base-char
             (and (not vector) (simple-array base-char)))
            (core:simple-mdarray-character
             (and (not vector) (simple-array character)))
            (core:simple-mdarray-t
             (and (not vector) (simple-array t)))

            (core:mdarray (and array (not vector)))
            (core:mdarray-bit
             (and (not simple-array) (not vector) (array bit)))
            (core:mdarray-fixnum
             (and (not simple-array) (not vector) (array fixnum)))
            (core:mdarray-byte2-t
             (and (not simple-array) (not vector) (array ext:byte2)))
            (core:mdarray-byte4-t
             (and (not simple-array) (not vector) (array ext:byte4)))
            (core:mdarray-byte8-t
             (and (not simple-array) (not vector) (array ext:byte8)))
            (core:mdarray-byte16-t
             (and (not simple-array) (not vector) (array ext:byte16)))
            (core:mdarray-byte32-t
             (and (not simple-array) (not vector) (array ext:byte32)))
            (core:mdarray-byte64-t
             (and (not simple-array) (not vector) (array ext:byte64)))
            (core:mdarray-int2-t
             (and (not simple-array) (not vector) (array ext:integer2)))
            (core:mdarray-int4-t
             (and (not simple-array) (not vector) (array ext:integer4)))
            (core:mdarray-int8-t
             (and (not simple-array) (not vector) (array ext:integer8)))
            (core:mdarray-int16-t
             (and (not simple-array) (not vector) (array ext:integer16)))
            (core:mdarray-int32-t
             (and (not simple-array) (not vector) (array ext:integer32)))
            (core:mdarray-int64-t
             (and (not simple-array) (not vector) (array ext:integer64)))
            (core:mdarray-float
             (and (not simple-array) (not vector) (array single-float)))
            (core:mdarray-double
             (and (not simple-array) (not vector) (array double-float)))
            (core:mdarray-base-char
             (and (not simple-array) (not vector) (array base-char)))
            (core:mdarray-character
             (and (not simple-array) (not vector) (array character)))
            (core:mdarray-t
             (and (not simple-array) (not vector) (array t))))
  #+sbcl ()
  #+ccl ()
  #-(or clasp sbcl ccl) (error "CLASS-ALIASES not defined for implementation")
  :test #'equal)

(defun subclassp (sub super)
  #+clasp (core:subclassp sub super)
  #+sbcl (member super (sb-mop:class-precedence-list sub))
  #+ccl (ccl::subclassp sub super)
  #+sicl (member super (sicl-clos:class-precedence-list sub))
  #-(or clasp sbcl ccl sicl) (error "SUBCLASSP not defined for implementation"))

(defun typexpand (type-specifier environment)
  #+clasp (cleavir-env:type-expand environment type-specifier)
  #+sbcl (sb-ext:typexpand type-specifier environment)
  #+ccl (ccl::type-expand type-specifier environment)
  #+sicl (funcall (sicl-environment:fdefinition
                   (sicl-environment:client environment)
                   environment
                   'sicl-type:type-expander)
                  type-specifier)
  #-(or clasp sbcl ccl sicl) (error "TYPEXPAND not defined for implementation"))

;;; Below, the idea is that (typep object '(complex foo)) is equivalent to
;;; (complex-ucptp object ufoo), where ufoo is (upgraded-complex-part-type 'foo)
(defmacro complex-ucptp (objectf ucpt)
  (declare (ignorable objectf))
  `(ecase ,ucpt
     ((*) t)
     #+clasp ,@()
     #+sbcl ((single-float) (sb-kernel:complex-single-float-p ,objectf))
     #+sbcl ((double-float) (sb-kernel:complex-double-float-p ,objectf))
     #+sbcl ((rational) (sb-kernel:complex-rational-p ,objectf))
     #+ccl ((single-float) (ccl::complex-single-float-p ,objectf))
     #+ccl ((double-float) (ccl::complex-double-float-p ,objectf))
     #-(or clasp sbcl ccl) ,(error "COMPLEX-UCPTP not defined for implementation")))

;;;

(defconstant +distinct-short-float-zeroes-p+ (not (eql -0s0 0s0)))
(defconstant +distinct-single-float-zeroes-p+ (not (eql -0f0 0f0)))
(defconstant +distinct-double-float-zeroes-p+ (not (eql -0d0 0d0)))
(defconstant +distinct-long-float-zeroes-p+ (not (eql -0L0 0L0)))

(defmacro range-kindp (objectf kindf)
  `(ecase ,kindf
     ((integer) (integerp ,objectf))
     ((ratio) (ratiop ,objectf))
     ,@(loop for (kind . pred) in +floats+
             collect `((,kind) (,pred ,objectf)))))
