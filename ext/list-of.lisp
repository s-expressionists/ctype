(defpackage #:ctype.ext.list-of
  (:use #:cl #:ctype)
  (:export #:list-of))

(in-package #:ctype.ext.list-of)

(defclass list-of (ctype)
  ((%etype :initarg :etype :reader element-ctype)))

(defun list-of (ect)
  (if (bot-p ect)
      (cmember nil)
      (make-instance 'list-of :etype ect)))

(defmethod ctypep ((object null) (ct list-of)) t)
(defmethod ctypep ((object cons) (ct list-of))
  (let ((ect (element-ctype ct)))
    (and (ctypep (car object) ect)
         ;; Traverse circular lists carefully
         (loop for sub = (cdr object) then (cdr sub)
               until (or (null sub) (eq sub object))
               unless (ctypep (car sub) ect)
                 return nil
               finally (return t)))))
(defmethod ctypep ((object t) (ct list-of)) nil)

(defmethod subctypep ((ct1 list-of) (ct2 list-of))
  (subctypep (element-ctype ct1) (element-ctype ct2)))
(defmethod ctype= ((ct1 list-of) (ct2 list-of))
  (ctype= (element-ctype ct1) (element-ctype ct2)))

(defmethod disjointp ((ct1 list-of) (ct2 list-of))
  (disjointp (element-ctype ct1) (element-ctype ct2)))
(defmethod conjointp ((ct1 list-of) (ct2 list-of)) (values nil t))

(defmethod cofinitep ((ct list-of)) (values nil t))

(defmethod conjoin/2 ((ct1 list-of) (ct2 list-of))
  (list-of (conjoin (element-ctype ct1) (element-ctype ct2))))
(defmethod disjoin/2 ((ct1 list-of) (ct2 list-of))
  (list-of (disjoin (element-ctype ct1) (element-ctype ct2))))

(defmethod subtract ((ct1 list-of) (ct2 list-of))
  (list-of (conjoin (element-ctype ct1) (negate (element-ctype ct2)))))

;;;

(defmethod subctypep ((ct1 list-of) (ct2 ccons))
  ;; list-of includes nil, a non-cons
  (values nil t))
(defmethod subctypep ((ct1 ccons) (ct2 list-of))
  ;; cons types are never recursive, so they can't be subtype of list-of
  ;; except in the degenerate cons type case.
  ;; (Note that we don't need to worry about degenerate list-of,
  ;;  since (list-of nil) is not nil, it's null.)
  (or/tri (subctypep (ccons-car ct1) (bot))
          (subctypep (ccons-cdr ct2) (bot))))

(defmethod subctypep ((ct1 cmember) (ct2 list-of))
  (values (equal (cmember-members ct1) '(nil)) t))

(defun clo-disjointp (ccons list-of)
  (or/tri (disjointp (ccons-car ccons) (element-ctype list-of))
          (disjointp (ccons-cdr ccons) list-of)))
(defmethod disjointp ((ct1 list-of) (ct2 ccons)) (clo-disjointp ct2 ct1))
(defmethod disjointp ((ct1 ccons) (ct2 list-of)) (clo-disjointp ct1 ct2))

(defun mlo-disjointp (cmember)
  (values (not (member nil (cmember-members cmember))) t))
(defmethod disjointp ((ct1 list-of) (ct2 cmember)) (mlo-disjointp ct2))
(defmethod disjointp ((ct1 cmember) (ct2 list-of)) (mlo-disjointp ct1))

(macrolet ((defexclusive (class)
             `(progn
                (defmethod subctypep ((ct1 list-of) (ct2 ,class))
                  (values nil t))
                (defmethod subctypep ((ct1 ,class) (ct2 list-of))
                  (values nil t))
                (defmethod disjointp ((ct1 list-of) (ct2 ,class))
                  (values t t))
                (defmethod disjointp ((ct1 ,class) (ct2 list-of))
                  (values t t))
                (defmethod conjointp ((ct1 list-of) (ct2 ,class))
                  (values nil t))
                (defmethod conjointp ((ct1 ,class) (ct2 list-of))
                  (values nil t))))
           (defexclusives (&rest classes)
             `(progn ,@(loop for class in classes
                             collect `(defexclusive ,class)))))
  (defexclusives range ccomplex carray charset cfunction fpzero))

(defmethod conjointp ((ct1 list-of) (ct2 cclass)) (values nil t))
(defmethod conjointp ((ct1 cclass) (ct2 list-of)) (values nil t))

;;; LIST is a subtype of SEQUENCE, so all LIST-OF types are as well.
(defun sequence-cclass-p (cclass)
  (eq (class-name (cclass-class cclass)) 'sequence))
(defmethod subctypep ((ct1 list-of) (ct2 cclass))
  (values (sequence-cclass-p ct2) t))
(defmethod subctypep ((ct1 cclass) (ct2 list-of)) (values nil t))
(defmethod disjointp ((ct1 list-of) (ct2 cclass))
  (values (not (sequence-cclass-p ct2)) t))
(defmethod disjointp ((ct1 cclass) (ct2 list-of))
  (values (not (sequence-cclass-p ct2)) t))
(defmethod conjoin/2 ((ct1 list-of) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct1 (bot)))
(defmethod conjoin/2 ((ct1 cclass) (ct2 list-of))
  (if (sequence-cclass-p ct1) ct2 (bot)))
(defmethod disjoin/2 ((ct1 list-of) (ct2 cclass))
  (if (sequence-cclass-p ct2) ct2 nil))
(defmethod disjoin/2 ((ct1 cclass) (ct2 list-of))
  (if (sequence-cclass-p ct1) ct1 nil))
(defmethod subtract ((ct1 list-of) (ct2 cclass))
  (if (sequence-cclass-p ct2) (bot) ct1))
(defmethod subtract ((ct1 cclass) (ct2 list-of))
  (if (sequence-cclass-p ct1) nil (bot)))
