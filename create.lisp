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
  (make-instance 'ccons :car car :cdr cdr))

(defun cmember (&rest members)
  (make-instance 'cmember :members members))
