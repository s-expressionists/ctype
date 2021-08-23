(in-package #:ctype)

(declaim (inline ratiop))
(defun ratiop (object) (typep object 'ratio))

(define-constant +floats+
    '((single-float . sicl-arithmetic:single-float-p)
      (double-float . sicl-arithmetic:double-float-p))
  :test #'equal)

(define-constant +standard-charset+ '((10 . 10) (32 . 126)) :test #'equal)
(define-constant +base-charset+ '((0 . #x10FFFF)) :test #'equal)

(define-constant +string-uaets+ '(character) :test #'equal)

(define-constant +complex-arrays-exist-p+ nil)

(declaim (inline simple-array-p))
(defun simple-array-p (object) (arrayp object))

(define-constant +class-aliases+
    '((sicl-array:array-t (array t))
      (sicl-array:array-bit (array bit))
      (sicl-array:array-unsigned-byte-8 (array (unsigned-byte 8)))
      (sicl-array:array-unsigned-byte-32 (array (unsigned-byte 32)))
      (sicl-array:array-signed-byte-32 (array (signed-byte 32)))
      (sicl-array:array-unsigned-byte-64 (array (unsigned-byte 64)))
      (sicl-array:array-signed-byte-64 (array (signed-byte 64)))
      (sicl-array:array-character (array character))
      (sicl-array:array-single-float (array single-float))
      (sicl-array:array-double-float (array single-float))
      (sicl-array:array-complex-single-float (array (complex single-float)))
      (sicl-array:array-complex-double-float (array (complex double-float)))
      (sicl-array:vector-unsigned-byte-8 (vector (unsigned-byte 8)))
      (sicl-array:vector-unsigned-byte-32 (vector (unsigned-byte 32)))
      (sicl-array:vector-signed-byte-32 (vector (signed-byte 32)))
      (sicl-array:vector-unsigned-byte-64 (vector (unsigned-byte 64)))
      (sicl-array:vector-signed-byte-64 (vector (signed-byte 64))))
  :test #'equal)

(declaim (inline subclassp))
(defun subclassp (sub super)
  (member super (sicl-clos:class-precedence-list sub)))

(defun typexpand (type-specifier environment)
  (funcall (sicl-environment:fdefinition
            (sicl-environment:client environment)
            environment
            'sicl-type:type-expander)
           type-specifier))

(defmacro complex-ucptp (objectf ucpt)
  `(ecase ,ucpt
     ((*) t)
     ((single-float) (typep ,objectf 'sicl-arithmetic:complex-single-float))
     ((double-float) (typep ,objectf 'sicl-arithmetic:complex-double-float))
     ((rational) (typep ,objectf 'sicl-arithmetic:complex-rational))))
