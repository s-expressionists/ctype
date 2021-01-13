(in-package #:ctype)

;;;; Pair methods
;;;; That is, methods on two specific ctype classes.

;;; cclass ctypes are excluded from several other ctypes (when things are
;;; normalized correctly), so we can mark their conjunctions as empty, etc.

(defmacro defexclusive/2 (class1 class2)
  `(progn
     (defmethod subctypep ((ct1 ,class1) (ct2 ,class2)) (values nil t))
     (defmethod subctypep ((ct1 ,class2) (ct2 ,class1)) (values nil t))
     (defmethod conjoin/2 ((ct1 ,class1) (ct2 ,class2)) (bot))
     (defmethod conjoin/2 ((ct1 ,class2) (ct2 ,class1)) (bot))
     (defmethod subtract ((ct1 ,class1) (ct2 ,class2)) ct1)
     (defmethod subtract ((ct2 ,class2) (ct1 ,class1)) ct2)))

(defmacro defexclusive (&rest classes)
  `(progn
     ,@(loop for (class1 . rest) on classes
             nconc (loop for class2 in rest
                         collect `(defexclusive/2 ,class1 ,class2)))))

(defexclusive cclass ccons range ccomplex carray)

;;; Some ctypes represent an infinite number of possible objects, so they are
;;; never subctypes of any member ctype.

(defmacro definfinite (class)
  `(defmethod subctypep ((ct1 ,class) (ct2 cmember)) (values nil t)))

(definfinite cclass)
(definfinite ccons)
(definfinite range)
(definfinite ccomplex)
(definfinite carray)
