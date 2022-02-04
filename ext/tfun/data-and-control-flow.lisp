(in-package #:ctype.ext.tfun)

;;; approximate
(defun append-values (vty1 vty2)
  (let ((r1 (cvalues-required vty1)) (o1 (cvalues-optional vty1))
        (r2 (cvalues-required vty2)) (o2 (cvalues-optional vty2))
        (rest1 (cvalues-rest vty1)) (rest2 (cvalues-rest vty2)))
    (if (and (null o1) (bot-p rest1))
        (cvalues (append r1 r2) o2 rest2) ; simple exact case
        (cvalues (append r1 (make-list (length r2) :initial-element (top)))
                 nil (top)))))

(defgeneric tfuncall (ftype argstype))
(defmethod tfuncall ((ftype ctype) (argstype cvalues)) (values-top))
(defmethod tfuncall ((ftype conjunction) (argstype cvalues))
  (apply #'conjoin (mapcar (lambda (f) (tfuncall f argstype))
                           (junction-ctypes ftype))))
(defmethod tfuncall ((ftype disjunction) (argstype cvalues))
  (apply #'disjoin (mapcar (lambda (f) (tfuncall f argstype))
                           (junction-ctypes ftype))))
(defmethod tfuncall ((ftype cfunction) (argstype cvalues))
  (declare (ignore argstype)) ; FIXME?: could check it against the LL
  (cfunction-returns ftype))
(defmethod tfuncall ((ftype tfun) (argstype cvalues))
  (funcall (deriver ftype) argstype))

;;; Exported interface functions
(defun derive-call (ftype &rest argtypes)
  (tfuncall ftype (cvalues argtypes nil (bot))))
(defun derive-multiple-value-call (ftype &rest formtypes)
  (cond ((null formtypes) (tfuncall ftype (cvalues nil nil (bot))))
        ((null (rest formtypes)) (tfuncall ftype (first formtypes)))
        (t (tfuncall ftype (reduce #'append-values formtypes)))))

(define-tfun funcall (function &rest args) (tfuncall function args))

(define-tfun apply (fun &rest vtype)
  ;; We try to derive the type based on the function being called,
  ;; by treating (apply f x ... z) as
  ;; (multiple-value-call f (values x) ... (values-list z))
  (let* ((req (cvalues-required vtype))
         (opt (cvalues-optional vtype))
         (rest (cvalues-rest vtype))
         (fargstype
           (cond ((and (not (null req)) (null opt) (bot-p rest))
                  ;; We have the exact arguments to apply, so we can work
                  ;; out the type of the arguments to the function easily.
                  (append-values (cvalues (butlast req) nil (bot))
                                 (tvalues-list (first (last req)))))
                 (t
                  ;; We could do better at the general case, but for now,
                  ;; mostly just punt except getting as many arguments as
                  ;; possible that definitely aren't the final list.
                  (cvalues (butlast req) nil (top))))))
    (tfuncall fun fargstype)))

;;;

(define-tfun not (x)
  (let ((null (specifier-ctype 'null)))
    (cond ((disjointp x null) null)
          ((subctypep x null) (negate null))
          (t (top)))))

(define-tfun eql (x y)
  (let ((null (specifier-ctype 'null)))
    (single-value
     (cond ((disjointp x y) null)
           #+(or)
           ((and (constantp x) (constantp y)
                 (eql (constant-value x) (constant-value y)))
            (negate null))
           (t (top))))))

(define-tfun identity (x) (single-value x))

(define-tfun values (&rest input)
  ;; VALUES is IDENTITY on the values level
  input)

(defgeneric tvalues-list (list))
(defmethod tvalues-list ((list ctype)) (values-top))
(defmethod tvalues-list ((list conjunction))
  (apply #'conjoin (mapcar #'tvalues-list (junction-ctypes list))))
(defmethod tvalues-list ((list disjunction))
  (apply #'disjoin (mapcar #'tvalues-list (junction-ctypes list))))
(defmethod tvalues-list ((list ccons))
  (append-values (cvalues (list (ccons-car list)) () (bot))
                 (tvalues-list (ccons-cdr list))))
(defmethod tvalues-list ((list cmember))
  (if (member nil (cmember-members list))
      (cvalues () () (bot))
      (values-bot)))

(define-tfun values-list (list) (tvalues-list list))
