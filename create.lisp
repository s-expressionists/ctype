(in-package #:ctype)

;;; Make conjunctions and disjunctions.
(defun conjunction (&rest ctypes)
  (cond ((null ctypes) (top))
        ((null (rest ctypes)) (first ctypes))
        (t (make-instance 'conjunction :ctypes ctypes))))
(defun disjunction (&rest ctypes)
  (cond ((null ctypes) (bot))
        ((null (rest ctypes)) (first ctypes))
        (t (make-instance 'disjunction :ctypes ctypes))))

;;; top and bottom types are special cases, so we give them particular
;;; identities. Normalized top ctypes must be identical to (top) and
;;; normalized bottom ctypes must be identical to (bot).

(defvar *top* (make-instance 'conjunction :ctypes nil) #+(or)(conjunction nil))
(defvar *bot* (make-instance 'disjunction :ctypes nil) #+(or)(disjunction nil))

(defun top () *top*)
(defun top-p (ctype) (eq ctype *top*))

(defun bot () *bot*)
(defun bot-p (ctype) (eq ctype *bot*))

;;; Others
(defun ccons (car cdr)
  (if (or (bot-p car) (bot-p cdr))
      (bot)
      (make-instance 'ccons :car car :cdr cdr)))

(defun charset (pairs)
  (if (null pairs)
      (bot)
      (make-instance 'charset :pairs pairs)))

(defun carray (simplicity uaet dims)
  (make-instance 'carray :simplicity simplicity :uaet uaet :dims dims))

(defun cmember (&rest members)
  (if members
      (make-instance 'cmember :members members)
      (bot)))

(defun cvalues (required optional rest)
  (if (and (null required)
           (every #'top-p optional)
           (top-p rest))
      (top)
      (make-instance 'cvalues
        :required required :optional optional :rest rest)))
