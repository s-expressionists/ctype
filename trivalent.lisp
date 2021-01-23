(in-package #:ctype)

;;; If the predicate is true of all sequence members, returns T T.
;;; If it is definitely false on at least one member, returns NIL T.
;;; Otherwise returns NIL NIL.
(defun every/tri (predicate sequence)
  (loop with surety = t
        for elem in sequence
        do (multiple-value-bind (val subsurety)
               (funcall predicate elem)
             (cond ((not subsurety) (setf surety nil))
                   ((not val) (return (values nil t)))))
        finally (return (if surety (values t t) (values nil nil)))))

;;; If the predicate is definitely false of all sequence members, NIL T.
;;; If true of at least one member, T T.
;;; Otherwise NIL NIL.
(defun some/tri (predicate sequence)
  (loop with surety = t
        for elem in sequence
        do (multiple-value-bind (val subsurety)
               (funcall predicate elem)
             (cond ((not subsurety) (setf surety nil))
                   (val (return (values t t)))))
        finally (return (values nil surety))))

;;; If true of all, NIL T.
;;; If false of at least one member, T T.
;;; Otherwise NIL NIL.
(defun notevery/tri (predicate sequence)
  (loop with surety = t
        for elem in sequence
        do (multiple-value-bind (val subsurety)
               (funcall predicate elem)
             (cond ((not subsurety) (setf surety nil))
                   ((not val) (return (values t t)))))
        finally (return (values nil surety))))

;;; Like AND, but returns both values.
;;; i.e., if a form returns false, returns those two values immediately.
;;; if all forms are true, returns the two values of the last form.
;;; Otherwise returns unknown.
(defmacro and/tri (&rest forms)
  (cond ((null forms) '(values t t))
        ((null (rest forms)) (first forms))
        (t (let ((val1 (gensym "VAL")) (surety1 (gensym "SURETY"))
                 (val2 (gensym "VAL")) (surety2 (gensym "SURETY")))
             `(multiple-value-bind (,val1 ,surety1) ,(first forms)
                (if (and ,surety1 (not ,val1))
                    (values nil ,surety1)
                    (multiple-value-bind (,val2 ,surety2)
                        (and/tri ,@(rest forms))
                      (if ,val1 ; = (and ,val1 ,surety1)
                          (values ,val2 ,surety2)
                          (values nil nil)))))))))

;;; Like OR, but returns both values.
;;; i.e., if a form returns true, returns those two values immediately.
;;; If all forms are false, returns the two values of the last form.
;;; Otherwise returns unknown.
(defmacro or/tri (&rest forms)
  (cond ((null forms) '(values nil t))
        ((null (rest forms)) (first forms))
        (t (let ((val1 (gensym "VAL")) (surety1 (gensym "SURETY"))
                 (val2 (gensym "VAL")) (surety2 (gensym "SURETY")))
             `(multiple-value-bind (,val1 ,surety1) ,(first forms)
                (if ,val1
                    (values ,val1 ,surety1)
                    (multiple-value-bind (,val2 ,surety2)
                        (or/tri ,@(rest forms))
                      (if ,surety1 ; = (and (not ,val1) ,surety1)
                          (values ,val2 ,surety2)
                          (values nil nil)))))))))

;;; Evaluate the forms left to right until one has sure results; return those
;;; results. If no form has surety just returns NIL NIL.
(defmacro surely (&rest forms)
  (cond ((null forms) '(values nil nil))
        ((null (rest forms)) (first forms))
        (t (let ((val (gensym "VAL")) (surety (gensym "SURETY")))
             `(multiple-value-bind (,val ,surety) ,(first forms)
                (if ,surety
                    (values ,val ,surety)
                    (surely ,@(rest forms))))))))
