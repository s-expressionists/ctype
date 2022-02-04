(in-package #:ctype.ext.tfun)

(defun single-value (ctype)
  (cvalues (list ctype) nil (bot)))

(defun constant-type-p (ctype)
  (and (typep ctype 'cmember)
       (= (length (cmember-members ctype)) 1)))

(defun constant-type-value (ctype)
  (first (cmember-members ctype)))
