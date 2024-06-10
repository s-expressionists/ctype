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

(defgeneric tfuncall (client ftype argstype))
(defmethod tfuncall (client (ftype ctype) (argstype cvalues))
  (declare (ignore client))
  (values-top))
(defmethod tfuncall (client (ftype conjunction) (argstype cvalues))
  (apply #'conjoin client (mapcar (lambda (f) (tfuncall client f argstype))
                                  (junction-ctypes ftype))))
(defmethod tfuncall (client (ftype disjunction) (argstype cvalues))
  (apply #'disjoin client (mapcar (lambda (f) (tfuncall client f argstype))
                                  (junction-ctypes ftype))))
(defmethod tfuncall (client (ftype cfunction) (argstype cvalues))
  (declare (ignore client argstype)) ; FIXME?: could check it against the LL
  (cfunction-returns ftype))
(defmethod tfuncall (client (ftype tfun) (argstype cvalues))
  (funcall (deriver ftype) client argstype))

;;; Exported interface functions
(defun derive-call (client ftype &rest argtypes)
  (tfuncall client ftype (cvalues argtypes nil (bot))))
(defun derive-multiple-value-call (client ftype &rest formtypes)
  (cond ((null formtypes) (tfuncall client ftype (cvalues nil nil (bot))))
        ((null (rest formtypes)) (tfuncall client ftype (first formtypes)))
        (t (tfuncall client ftype (reduce #'append-values formtypes)))))

(define-tfun funcall (client function &rest args) (client tfuncall function args))

(define-tfun apply (client fun &rest vtype)
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
                                 (tvalues-list client (first (last req)))))
                 (t
                  ;; We could do better at the general case, but for now,
                  ;; mostly just punt except getting as many arguments as
                  ;; possible that definitely aren't the final list.
                  (cvalues (butlast req) nil (top))))))
    (tfuncall client fun fargstype)))

;;;

(define-tfun not (client x)
  (let ((null (specifier-ctype client 'null)))
    (cond ((disjointp client x null) null)
          ((subctypep client x null) (negate client null))
          (t (top)))))

(define-tfun eql (c x y)
  (let ((null (specifier-ctype c 'null)))
    (single-value
     (cond ((disjointp c x y) null)
           #+(or)
           ((and (constantp x) (constantp y)
                 (eql (constant-value x) (constant-value y)))
            (negate c null))
           (t (top))))))

(define-tfun identity (c x) (single-value x))

(define-tfun values (c &rest input)
  ;; VALUES is IDENTITY on the values level
  input)

(defgeneric tvalues-list (client list))
(defmethod tvalues-list (client (list ctype))
  (declare (ignore client))
  (values-top))
(defmethod tvalues-list (c (list conjunction))
  (apply #'conjoin c (mapcar (lambda (ct) (tvalues-list c ct))
                             (junction-ctypes list))))
(defmethod tvalues-list (c (list disjunction))
  (apply #'disjoin c (mapcar #'tvalues-list (junction-ctypes list))))
(defmethod tvalues-list (c (list ccons))
  (append-values (cvalues (list (ccons-car list)) () (bot))
                 (tvalues-list c (ccons-cdr list))))
(defmethod tvalues-list (c (list cmember))
  (declare (ignore c))
  (if (member nil (cmember-members list))
      (cvalues () () (bot))
      (values-bot)))

(define-tfun values-list (client list) (tvalues-list client list))
