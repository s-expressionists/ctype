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

(defconstant +ratiop+
  #+clasp 'ext::ratiop
  #-(or clasp) (error "RATIOP not defined for implementation"))

(define-constant +floats+
  #+clasp '((single-float . core:single-float-p)
            (double-float . core:double-float-p))
  #-(or clasp) (error "FLOATS not defined for implementation")
  :test #'equal)

(define-constant +string-uaets+
  #+clasp '(base-char character)
  #-(or clasp) (error "STRING-UAETS not defind for implementation")
  :test #'equal)

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
  #-(or clasp) (error "CLASS-ALIASES not defined for implementation")
  :test #'equal)

(defun subclassp (sub super)
  #+clasp (core:subclassp sub super)
  #+(or) (member super (mop:class-precedence-list sub))
  #-(or clasp) (error "SUBCLASSP not defined for implementation"))

(defun typexpand (type-specifier environment)
  #+clasp (cleavir-env:type-expand environment type-specifier)
  #-(or clasp) (error "TYPEXPAND not defined for implementation"))

(defmacro complex-ucptp (objectf ucpt)
  (declare (ignorable objectf))
  `(ecase ,ucpt
     ((*) t)
     #+clasp ,@()
     #-(or clasp) ,(error "COMPLEX-UCPTP not defined for implementation")))

;;;

(defmacro range-kindp (objectf kindf)
  `(ecase ,kindf
     ((integer) (integerp ,objectf))
     ((ratio) (,+ratiop+ ,objectf))
     ,@(loop for (kind . pred) in +floats+
             collect `((,kind) (,pred ,objectf)))))
