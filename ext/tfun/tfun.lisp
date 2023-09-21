(in-package #:ctype.ext.tfun)

(defclass tfun (ctype)
  ((%deriver :initarg :deriver :reader deriver :type cl:function)))

(defun tfun (deriver)
  (make-instance 'tfun :deriver deriver))

(defmethod conjointp ((ct1 tfun) (ct2 tfun)) (values nil t))
(defmethod cofinitep ((ct tfun)) (values nil t))

(defmethod subctypep ((ct1 tfun) (ct2 cfunction))
  (if (function-top-p ct2) (values t t) (values nil nil)))
(defmethod subctypep ((ct1 cfunction) (ct2 tfun)) (values nil t))
(define-commutative-method disjointp ((ct1 tfun) (ct2 cfunction))
  (if (function-top-p ct2) (values nil t) (values nil nil)))
(define-commutative-method conjointp ((ct1 tfun) (ct2 cfunction))
  (values nil t))

(defexclusives tfun cclass ccomplex carray charset fpzero range)

;;;

(defmacro destructure-tfunbind (lambda-list values-type &body body)
  "Bind LAMBDA-LIST to VALUES-TYPE for the duration of BODY.
LAMBDA-LIST is an ordinary lambda list, except that default forms and suppliedp variables are ignored.
VALUES-TYPE is evaluated to a values ctype.
Variables in the lambda list are bound to the values. For example, with a lambda list of (A B) and type (values integer &optional cons), A would be bound to the INTEGER ctype and B to the CONS ctype. The &rest parameter is bound to a values ctype containing all values not used by the required and optional parameters.
(values-bot) is returned without the body being evaluated in any of the following situations:
* There are fewer values than there are required parameters (i.e. subsequent values are bottom)
* There is no &rest parameter and there are more required values than there are required and optional parameters
* &allow-other-keys was specified, it can be determined that :allow-other-keys true was not passed, and there is an unrecognized keyword"
  (multiple-value-bind (required optional rest keys aokp aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore keys))
    (when aokp
      (error "~s not allowed in TFUN lambda list" '&allow-other-keys))
    (when aux
      (error "~s not allowed in TFUN lambda list" '&aux))
    (when keyp (error "~s not supported yet" '&key))
    (let ((optional
            (loop for (var init suppliedp) in optional
                  when (or init suppliedp)
                    do (warn "~s ignoring initform and/or suppliedp variable for ~s"
                             'destructure-tfunbind var)
                  collect var))
          (vtype (gensym "VALUES-TYPE"))
          (vreq (gensym "REQUIRED"))
          (vopt (gensym "OPTIONAL"))
          (vrest (gensym "REST")))
      `(let* ((,vtype ,values-type)
              (,vreq (cvalues-required ,vtype))
              (,vopt (cvalues-optional ,vtype))
              (,vrest (cvalues-rest ,vtype)))
         (let (,@(loop for r in required
                       collect `(,r (or (pop ,vreq) (pop ,vopt) ,vrest)))
               ,@(loop for o in optional
                       collect `(,o (or (pop ,vreq) (pop ,vopt) ,vrest)))
               ,@(when rest
                   `((,rest (cvalues ,vreq ,vopt ,vrest)))))
           ;; declarations are annoying to work out, so
           (declare (ignorable ,@required ,@optional ,@(when rest `(,rest))))
           ;; Enforce too many args, strictness, too few args
           (cond ,@(unless rest
                     `((,vreq (values-bot)))) ; too many (vreq remains)
                 ,@(loop for r in required ; too few
                         collect `((bot-p ,r) (values-bot)))
                 ;; specific code
                 (t ,@body)))))))

(defmacro tlambda (name (&rest lambda-list) &body body)
  (let ((vtype (gensym "VALUES-TYPE")))
    `(lambda (,vtype)
       (block ,name
         (destructure-tfunbind (,@lambda-list) ,vtype ,@body)))))

(defparameter *tfuns* (make-hash-table :test #'equal))

(defmacro define-tfun (name (&rest lambda-list) &body body)
  `(setf (gethash ',name *tfuns*) (tfun (tlambda ,name ,lambda-list ,@body))))

(defun find-tfun (name &optional errorp)
  (cond ((gethash name *tfuns*))
        (errorp (error "No TFUN known for ~s" name))
        (t (function-top))))
