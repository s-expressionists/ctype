(in-package #:ctype.test)

;;; Various helpers from ANSI tests' ansi-aux.lsp, universe.lsp, etc.

(defun notnot (x) (not (not x)))

;;; The function SUBTYPEP should return two generalized booleans.
;;; This auxiliary function returns booleans instead
;;; (which makes it easier to write tests).
;;; ctype subtypep should always return two booleans anyway,
;;; but we keep the looser semantics.
;;;
(defun subtypep* (type1 type2)
  (apply #'values
         (mapcar #'notnot
                 (multiple-value-list (subtypep type1 type2)))))

(defparameter *array-element-types*
  '(t (integer 0 0)
      bit (unsigned-byte 8) (unsigned-byte 16)
      (unsigned-byte 32) float short-float
      single-float double-float long-float
      nil character base-char symbol boolean null))

(defparameter +float-types+
  '(long-float double-float short-float single-float))

(defparameter *subtype-table*
(let ((table
       '(
         (null symbol)
         (symbol t)
         (boolean symbol)
         (standard-object t)
         (function t)
         (compiled-function function)
         (generic-function function)
         (standard-generic-function generic-function)
         (class standard-object)
         (built-in-class class)
         (structure-class class)
         (standard-class class)
         (method standard-object)
         (standard-method method)
         (structure-object t)
         (method-combination t)
         (condition t)
         (serious-condition condition)
         (error serious-condition)
         (type-error error)
         (simple-type-error type-error)
         (simple-condition condition)
         (simple-type-error simple-condition)
         (parse-error error)
         (hash-table t)
         (cell-error error)
         (unbound-slot cell-error)
         (warning condition)
         (style-warning warning)
         (storage-condition serious-condition)
         (simple-warning warning)
         (simple-warning simple-condition)
         (keyword symbol)
         (unbound-variable cell-error)
         (control-error error)
         (program-error error)
         (undefined-function cell-error)
         (package t)
         (package-error error)
         (random-state t)
         (number t)
         (real number)
         (complex number)
         (float real)
         (short-float float)
         (single-float float)
         (double-float float)
         (long-float float)
         (rational real)
         (integer rational)
         (ratio rational)
         (signed-byte integer)
         (integer signed-byte)
         (unsigned-byte signed-byte)
         (bit unsigned-byte)
         (fixnum integer)
         (bignum integer)
         (bit fixnum)
         (arithmetic-error error)
         (division-by-zero arithmetic-error)
         (floating-point-invalid-operation arithmetic-error)
         (floating-point-inexact arithmetic-error)
         (floating-point-overflow arithmetic-error)
         (floating-point-underflow arithmetic-error)
         (character t)
         (base-char character)
         (standard-char base-char)
         (extended-char character)
         (sequence t)
         (list sequence)
         (null list)
         (null boolean)
         (cons list)
         (array t)
         (simple-array array)
         (vector sequence)
         (vector array)
         (string vector)
         (bit-vector vector)
         (simple-vector vector)
         (simple-vector simple-array)
         (simple-bit-vector bit-vector)
         (simple-bit-vector simple-array)
         (base-string string)
         (simple-string string)
         (simple-string simple-array)
         (simple-base-string base-string)
         (simple-base-string simple-string)
         (pathname t)
         (logical-pathname pathname)
         (file-error error)
         (stream t)
         (broadcast-stream stream)
         (concatenated-stream stream)
         (echo-stream stream)
         (file-stream stream)
         (string-stream stream)
         (synonym-stream stream)
         (two-way-stream stream)
         (stream-error error)
         (end-of-file stream-error)
         (print-not-readable error)
         (readtable t)
         (reader-error parse-error)
         (reader-error stream-error)
         )))
  (when (subtypep* 'character 'base-char)
    (setq table
          (append
           '((character base-char)
             ;; (string base-string)
             ;; (simple-string simple-base-string)
             )
           table)))

  table))

(defparameter *condition-types*
    '(arithmetic-error
      cell-error
      condition
      control-error
      division-by-zero
      end-of-file
      error
      file-error
      floating-point-inexact
      floating-point-invalid-operation
      floating-point-underflow
      floating-point-overflow
      package-error
      parse-error
      print-not-readable
      program-error
      reader-error
      serious-condition
      simple-condition
      simple-error
      simple-type-error
      simple-warning
      storage-condition
      stream-error
      style-warning
      type-error
      unbound-slot
      unbound-variable
      undefined-function
      warning))

(defparameter *disjoint-types-list*
    '(cons symbol array
      number character hash-table function readtable package
      pathname stream random-state condition restart))

(defparameter *disjoint-types-list2*
  `((cons (cons t t) (cons t (cons t t)) (eql (nil)))
    (symbol keyword boolean null (eql a) (eql nil) (eql t) (eql *))
    (array vector simple-array simple-vector string simple-string
           base-string simple-base-string (eql #()))
    (character base-char standard-char (eql #\a)
               ,@(if (subtypep 'character 'base-char) nil
                   (list 'extended-char)))
    (function compiled-function generic-function standard-generic-function
              (eql ,#'car))
    (package (eql ,(find-package "COMMON-LISP")))
    (pathname logical-pathname (eql #p""))
    (stream broadcast-stream concatenated-stream echo-stream
            file-stream string-stream synonym-stream two-way-stream)
    (number real complex float integer rational ratio fixnum
            bit (integer 0 100) (float 0.0 100.0) (integer 0 *)
            (rational 0 *) (mod 10)
            (eql 0)
            ,@(and (not (subtypep 'bignum nil))
                   (list 'bignum)))
    (random-state)
    ,*condition-types*
    (restart)
    (readtable)))

(defparameter *types-list3*
  (reduce #'append *disjoint-types-list2* :from-end t))

(defun trim-list (list n)
  (let ((len (length list)))
    (if (<= len n) list
      (append (subseq list 0 n)
              (format nil "And ~A more omitted." (- len n))))))

(defun is-builtin-class (type)
  (when (symbolp type) (setq type (find-class type nil)))
  (typep type 'built-in-class))

(defun classes-are-disjoint (c1 c2)
  "If either c1 or c2 is a builtin class or the name of a builtin
   class, then check for disjointness.  Return a non-NIL list
   of failed subtypep relationships, if any."
  (and (or (is-builtin-class c1)
           (is-builtin-class c2))
       (check-disjointness c1 c2)))

(defun check-subtypep (type1 type2 is-sub &optional should-be-valid)
  (multiple-value-bind
      (sub valid)
      (subtypep type1 type2)
    (unless (constantp type1) (setq type1 (list 'quote type1)))
    (unless (constantp type2) (setq type2 (list 'quote type2)))
    (if (or (and valid sub (not is-sub))
            (and valid (not sub) is-sub)
            (and (not valid) should-be-valid))
        `(((SUBTYPEP ,type1 ,type2) :==> ,sub ,valid))
        nil)))

(defun check-disjointp (type1 type2 is-dis &optional should-be-valid)
  (multiple-value-bind (dis valid)
      (ctype:disjointp ctype-extrinsic:*client*
                       (ctype-extrinsic:specifier-ctype type1)
                       (ctype-extrinsic:specifier-ctype type2))
    (unless (constantp type1) (setq type1 (list 'quote type1)))
    (unless (constantp type2) (setq type2 (list 'quote type2)))
    (if (or (and valid dis (not is-dis))
            (and valid (not dis) is-dis)
            (and (not valid) should-be-valid))
        `(((CTYPE:DISJOINTP ,type1 ,type2) :==> ,dis ,valid))
        nil)))

(defun check-type= (type1 type2 is-type= &optional should-be-valid)
  (multiple-value-bind (type= valid)
      (ctype:ctype= ctype-extrinsic:*client*
                    (ctype-extrinsic:specifier-ctype type1)
                    (ctype-extrinsic:specifier-ctype type2))
    (unless (constantp type1) (setq type1 (list 'quote type1)))
    (unless (constantp type2) (setq type2 (list 'quote type2)))
    (if (or (and valid type= (not is-type=))
            (and valid (not type=) is-type=)
            (and (not valid) should-be-valid))
        `(((CTYPE:CTYPE= ,type1 ,type2) :==> ,type= ,valid))
        nil)))

;;; Check that the subtype relationships implied
;;; by disjointness are not contradicted.  Return NIL
;;; if ok, or a list of error messages if not.

;;; Assumes the types are nonempty.

(defun check-disjointness (type1 type2)
  (append
   (check-subtypep type1 type2 nil)
   (check-subtypep type2 type1 nil)
   (check-subtypep type1 `(not ,type2) t)
   (check-subtypep type2 `(not ,type1) t)
   (check-subtypep `(and ,type1 ,type2) nil t)
   (check-subtypep `(and ,type2 ,type1) nil t)
   (check-subtypep `(and ,type1 (not ,type2)) type1 t)
   (check-subtypep `(and (not ,type2) ,type1) type1 t)
   (check-subtypep `(and ,type2 (not ,type1)) type2 t)
   (check-subtypep `(and (not ,type1) ,type2) type2 t)
;;;   (check-subtypep type1 `(or ,type1 (not ,type2)) t)
;;;   (check-subtypep type1 `(or (not ,type2) ,type1) t)
;;;   (check-subtypep type2 `(or ,type2 (not ,type1)) t)
;;;   (check-subtypep type2 `(or (not ,type1) ,type2) t)
   (check-subtypep t `(or (not ,type1) (not ,type2)) t)
   (check-subtypep t `(or (not ,type2) (not ,type1)) t)
   (check-disjointp type1 type2 t)
   ))

(defun check-equivalence (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep type2 type1 t)
   (check-subtypep `(not ,type1) `(not ,type2) t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep `(and ,type2 (not ,type1)) nil t)
   (check-subtypep `(and (not ,type2) ,type1) nil t)
   (check-subtypep `(and (not ,type1) ,type2) nil t)
   (check-subtypep t `(or ,type1 (not ,type2)) t)
   (check-subtypep t `(or ,type2 (not ,type1)) t)
   (check-subtypep t `(or (not ,type2) ,type1) t)
   (check-subtypep t `(or (not ,type1) ,type2) t)
   (check-type= type1 type2 t)))

(defun check-all-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))

(defun check-all-not-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 nil)
   (check-subtypep `(not ,type2) `(not ,type1) nil)))

(defun subtypep-and-contrapositive-are-consistent (t1 t2)
  (multiple-value-bind (sub1 success1)
      (subtypep* t1 t2)
    (multiple-value-bind (sub2 success2)
        (subtypep* `(not ,t2) `(not ,t1))
      (or (not success1)
          (not success2)
          (eql sub1 sub2)))))
