(in-package #:ctype)

(flet ((expand (name invert-test-p success predicate sequences)
         ;; In separate function to avoid double backquote.
         (let* ((nsequences (length sequences))
                (params (loop repeat nsequences collect (gensym "ELEM")))
                (ssym (gensym "SURETY"))
                (psym (gensym "PREDICATE"))
                (msym (gensym "MAPPER")))
           `(block ,name
              (let ((,ssym t) (,psym ,predicate))
                (flet ((,msym (,@params)
                         (multiple-value-bind (val subsurety)
                             (funcall ,psym ,@params)
                           (cond ((not subsurety) (setf ,ssym nil))
                                 ((,(if invert-test-p 'not 'identity) val)
                                  (return-from ,name
                                    (values ,(not success) t)))))))
                  (declare (inline ,msym) (dynamic-extent (function ,msym)))
                  (map nil (function ,msym) ,@sequences)
                  (if ,ssym (values ,success t) (values nil nil))))))))
  (macrolet ((defpred (name invert-test-p success)
               `(progn
                  (defun ,name (predicate sequence &rest sequences)
                    (let ((surety t))
                      (flet ((mapper (&rest elems)
                               (multiple-value-bind (val subsurety)
                                   (apply predicate elems)
                                 (cond ((not subsurety) (setf surety nil))
                                       ((,(if invert-test-p 'not 'identity) val)
                                        (return-from ,name
                                          (values ,(not success) t)))))))
                        (declare (inline mapper) (dynamic-extent #'mapper))
                        (apply #'map nil #'mapper sequence sequences)
                        (if surety (values ,success t) (values nil nil)))))
                  ;; Open code to avoid the apply.
                  (define-compiler-macro ,name
                      (predicate sequence &rest sequences)
                    (expand ',name ',invert-test-p ',success predicate
                            (list* sequence sequences))))))
    ;; If the predicate is true of all sequence members, returns T T.
    ;; If it is definitely false on at least one member, returns NIL T.
    ;; Otherwise returns NIL NIL.
    ;; Like the CL map functions, only checks as many elements as the shortest
    ;; input, so make sure all inputs have the same length.
    (defpred every/tri t t)
    ;; If the predicate is definitely false of all sequence members, NIL T.
    ;; If true of at least one member, T T.
    ;; Otherwise NIL NIL.
    (defpred some/tri nil nil)
    ;; If true of all, NIL T.
    ;; If false of at least one member, T T.
    ;; Otherwise NIL NIL.
    (defpred notevery/tri t nil)
    ;; If false of all, T T.
    ;; If true of at least one member, NIL T.
    ;; Otherwise NIL NIL.
    (defpred notany/tri nil t)))

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
                      (if (or ,val1 (and ,surety2 (not ,val2)))
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
                      (if (or ,surety1 ,val2)
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
