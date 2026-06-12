(in-package #:ctype)

;;;; These are the functions that need to be specialized for CTYPEP to work,
;;;; rather than just defining the type system.
;;;; They cannot be implemented portably except through CL:TYPEP and CL:SUBTYPEP,
;;;; so any Lisp implementation using ctype to implement those functions must
;;;; provide its own specializations for these functions.

(defmethod range-kindp (client object kind)
  (declare (ignore client))
  (cl:typep object kind))
;; Some extras to be a tiny bit faster, maybe.
(macrolet ((defrange (kind)
             `(progn
                (defmethod range-kindp (client (object ,kind) (kind (eql ',kind)))
                  (declare (ignore client))
                  t)
                (defmethod range-kindp (client object (kind (eql ',kind)))
                  (declare (ignore client))
                  nil)))
           (defrange~ (kind &environment e)
             ;; define these methods, but only if the host actually has
             ;; classes for these types, which is not guaranteed.
             (if (cl:find-class kind nil e)
                 `(defrange ,kind)
                 nil)))
  (defrange integer)
  (defrange ratio)
  (defrange~ short-float)
  (defrange~ single-float)
  (defrange~ double-float)
  (defrange~ long-float))

(defmethod complex-ucptp (client (object complex) ucpt)
  (declare (ignore client) (ignorable ucpt))
  (cl:typep object `(complex ,ucpt)))
(defmethod complex-ucptp (client object ucpt)
  (declare (ignore client object ucpt))
  nil)

(macrolet ((defsap (&environment e)
             ;; simple-array is a type in the standard, so it may or may not
             ;; correspond to a class.
             (if (cl:find-class 'cl:simple-array nil e)
                 `(progn
                    (defmethod simple-array-p (client (object simple-array))
                      (declare (ignore client))
                      t)
                    (defmethod simple-array-p (client object)
                      (declare (ignore client object))
                      nil))
                 `(defmethod simple-array-p (client object)
                   (declare (ignore client))
                   (cl:typep object 'cl:simple-array)))))
  (defsap))

(defmethod subclassp (client (sub class) (super class))
  (declare (ignore client))
  (cl:subtypep sub super))
