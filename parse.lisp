(in-package #:ctype)

(defun array-ctype (simplicity et dims env)
  (let ((uaet (if (eq et '*)
                  et
                  (upgraded-array-element-type et env)))
        (eaet (if (eq et '*) (top) (specifier-ctype et env)))
        (dims (cond ((eq dims '*) dims)
                    ((and (integerp dims) (>= dims 0))
                     (make-list dims :initial-element '*))
                    ((and (listp dims)
                          (every (lambda (d) (or (and (integerp d) (>= d 0))
                                                 (eq d '*)))
                                 dims))
                     dims)
                    (t (error "Invalid dimension specification: ~a" dims)))))
    (if +complex-arrays-exist-p+
        (if (eq simplicity :either)
            (disjunction (carray :simple uaet eaet dims)
                         (carray :complex uaet eaet dims))
            (carray simplicity uaet eaet dims))
        (carray :simple uaet eaet dims))))

(defun cons-ctype (car cdr env)
  (let ((car (if (eq car '*)
                 (top)
                 (specifier-ctype car env)))
        (cdr (if (eq cdr '*)
                 (top)
                 (specifier-ctype cdr env))))
    (ccons car cdr)))

(defun member-ctype (elements)
  ;; Cut out real ranges, floating point zeroes, and character sets.
  (apply
   #'disjoin
   (loop with shortp = (cdr (assoc 'short-float +floats+))
         with singlep = (cdr (assoc 'single-float +floats+))
         with doublep = (cdr (assoc 'double-float +floats+))
         with longp = (cdr (assoc 'long-float +floats+))
         for elem in elements
         collect (cond ((integerp elem) (range 'integer elem nil elem nil))
                       ((ratiop elem) (range 'ratio elem nil elem nil))
                       ((characterp elem)
                        (let ((code (char-code elem)))
                          (charset (list (cons code code)))))
                       ((and shortp (funcall shortp elem))
                        (if (and +distinct-short-float-zeroes-p+ (zerop elem))
                            (fpzero 'short-float elem)
                            (range 'short-float elem nil elem nil)))
                       ((and singlep (funcall singlep elem))
                        (if (and +distinct-single-float-zeroes-p+ (zerop elem))
                            (fpzero 'single-float elem)
                            (range 'single-float elem nil elem nil)))
                       ((and doublep (funcall doublep elem))
                        (if (and +distinct-double-float-zeroes-p+ (zerop elem))
                            (fpzero 'double-float elem)
                            (range 'double-float elem nil elem nil)))
                       ((and longp (funcall longp elem))
                        (if (and +distinct-long-float-zeroes-p+ (zerop elem))
                            (fpzero 'long-float elem)
                            (range 'long-float elem nil elem nil)))
                       (t (cmember elem))))))

(defun error-interval-designator (nondesignator &optional kind)
  (error "~a is not a valid interval designator~@[ for type ~a~]"
         nondesignator kind))

(defun parse-interval-designator (designator)
  (cond ((eq designator '*) (values nil nil))
        ((realp designator) (values designator nil))
        ((and (consp designator) (null (cdr designator))
              (realp (car designator)))
         (values (car designator) t))
        (t (error-interval-designator designator))))

(defun rational-range (low lxp high hxp)
  ;; Check if this is a ratio-only range like (rational (0) (1))
  (if (and low high
           (or (< high (ceiling low))
               (and hxp
                    (or (= high (ceiling low))
                        (and (integerp low)
                             (= high (1+ low))
                             lxp)))))
      (range 'ratio low lxp high hxp)
      ;; Compute bounds for the integer aspect.
      (let ((ilow (cond ((null low) low)
                        ((integerp low) (if lxp (1+ low) low))
                        (t (ceiling low))))
            (ihigh (cond ((null high) high)
                         ((integerp high) (if hxp (1- high) high))
                         (t (floor high)))))
        (disjoin (range 'integer ilow nil ihigh nil)
                 (range 'ratio low lxp high hxp)))))

(defun float-range (low lxp high hxp)
  (let ((lj (loop for (ty) in +floats+
                  for nl = (if low (coerce low ty) low)
                  for nh = (if high (coerce high ty) high)
                  collect (range ty nl lxp nh hxp))))
    (apply #'disjoin lj)))

(defun range-ctype (kind low high env)
  (declare (ignore env))
  (let ((test (ecase kind
                ((integer) #'integerp)
                ((rational) #'rationalp)
                ((float) #'floatp)
                ((short-float)
                 (or (cdr (assoc 'short-float +floats+))
                     (cdr (assoc 'single-float +floats+))))
                ((single-float) (cdr (assoc 'single-float +floats+)))
                ((double-float)
                 (or (cdr (assoc 'double-float +floats+))
                     (cdr (assoc 'single-float +floats+))))
                ((long-float)
                 (or (cdr (assoc 'long-float +floats+))
                     (cdr (assoc 'double-float +floats+))
                     (cdr (assoc 'single-float +floats+))))
                ((real) #'realp))))
    (multiple-value-bind (nlow lxp) (parse-interval-designator low)
      (multiple-value-bind (nhigh hxp) (parse-interval-designator high)
        (unless (or (not nlow) (funcall test nlow))
          (error-interval-designator low kind))
        (unless (or (not nhigh) (funcall test nhigh))
          (error-interval-designator high kind))
        (ecase kind
          ((integer) (range 'integer nlow lxp nhigh hxp))
          ((rational) (rational-range nlow lxp nhigh hxp))
          ((float) (float-range nlow lxp nhigh hxp))
          ((short-float) (range (if (assoc 'short-float +floats+)
                                    'short-float
                                    'single-float)
                                nlow lxp nhigh hxp))
          ((single-float) (range 'single-float nlow lxp nhigh hxp))
          ((double-float) (range (if (assoc 'double-float +floats+)
                                     'double-float
                                     'single-float)
                                 nlow lxp nhigh hxp))
          ((long-float)
           (range (cond ((assoc 'long-float +floats+) 'long-float)
                        ((assoc 'double-float +floats+) 'double-float)
                        (t 'single-float))
                  nlow lxp nhigh hxp))
          ((real) (disjoin (float-range nlow lxp nhigh hxp)
                           (rational-range nlow lxp nhigh hxp))))))))

(defun complex-ctype (element-type env)
  (ccomplex (if (eq element-type '*)
                element-type
                (upgraded-complex-part-type element-type env))))

(defun %parse-lambda-list (ll env)
  (loop with state = :required
        with req with opt with rest with keyp with key with aokp
        for elem in ll
        do (case elem
             ((&optional)
              (unless (eq state :required)
                (error "Bad syntax in lambda-list ~a" ll))
              (setf state elem))
             ((&rest)
              (unless (member state '(:required &optional))
                (error "Bad syntax in lambda-list ~a" ll))
              (setf state elem))
             ((&key)
              (unless (member state '(:required &optional &rest))
                (error "Bad syntax in lambda-list ~a" ll))
              (setf keyp t state elem))
             ((&allow-other-keys)
              (unless (eq state '&key)
                (error "Bad syntax in lambda-list ~a" ll))
              (setf state elem))
             (t (ecase state
                  ((:required) (push (specifier-ctype elem env) req))
                  ((&optional) (push (specifier-ctype elem env) opt))
                  ((&rest)
                   (when rest (error "Bad syntax in lambda-list ~a" ll))
                   (setf rest (specifier-ctype elem env)))
                  ((&key)
                   (destructuring-bind (keyword spec) elem
                     (unless (symbolp keyword)
                       (error "Bad syntax in lambda-list ~a" ll))
                     (push (cons keyword (specifier-ctype spec env)) key)))
                  ((&allow-other-keys)
                   (error "Bad syntax in lambda-list ~a" ll)))))
        finally (return (values (nreverse req) (nreverse opt)
                                rest keyp key aokp))))

(defun parse-lambda-list (ll env)
  (multiple-value-bind (req opt rest keyp key aokp)
      (%parse-lambda-list ll env)
    ;; I was going to have some checks of whether any of the types are bot,
    ;; but probably that merits a warning... or osmething... instead?
    (make-instance 'lambda-list
      :required req :optional opt :rest (or rest (bot))
      :keyp keyp :keys key :aokp aokp)))

(defgeneric coerce-to-values (ctype))
(defmethod coerce-to-values ((ctype cvalues)) ctype)
(defmethod coerce-to-values ((ctype ctype))
  (cvalues (list ctype) nil (top)))

(defun function-ctype (ll rv env)
  (let ((ll (if (eq ll '*)
                (make-instance 'lambda-list
                  :required nil :optional nil :rest (top)
                  :keyp nil :keys nil :aokp nil)
                (parse-lambda-list ll env)))
        (rv (if (eq rv '*)
                (cvalues nil nil (top))
                (values-specifier-ctype rv))))
    (if (bot-p ll)
        ll
        (make-instance 'cfunction :lambda-list ll :returns rv))))

(defun %parse-values-ctype (vest env)
  (flet ((fail () (error "Bad syntax in values type: ~a" vest)))
    (loop with state = :required
          with req with opt with rest
          for elem in vest
          do (case elem
               ((&optional)
                (unless (eq state :required) (fail))
                (setf state elem))
               ((&rest)
                (unless (member state '(:required &optional)) (fail))
                (setf state elem))
               (otherwise
                (ecase state
                  ((:required) (push (specifier-ctype elem env) req))
                  ((&optional) (push (specifier-ctype elem env) opt))
                  ((&rest)
                   (when rest (fail))
                   (setf rest (specifier-ctype elem env))))))
          finally (return (values (nreverse req) (nreverse opt) rest)))))

(defun %fuzz-values-ctype (required optional rest)
  ;; CTYPE internally treats VALUES types with the strict semantics described
  ;; in the entry on the VALUES type. However, these semantics are not used in
  ;; any actual place in the language, and in particular, when used in THE
  ;; VALUES types are considerably vaguer. This function applies that vagueness:
  ;; (1) if &rest is not declared, &rest t is implicit
  ;; (2) if a suffix of the "required" types includes NULL, those values are
  ;;     not actually required.
  ;; If you want strict semantics, just make a CVALUES directly.
  (let* ((rest (or rest (top)))
         (rpos (position-if-not (lambda (ct) (ctypep nil ct))
                                required :from-end t))
         (rrpos (if rpos (1+ rpos) 0))
         (rreq (subseq required 0 rrpos))
         (ropt (append (nthcdr rrpos required) optional)))
    (values rreq ropt rest)))

(defun parse-values-ctype (rest env)
  (multiple-value-bind (req opt rest) (%parse-values-ctype rest env)
    (multiple-value-bind (req opt rest) (%fuzz-values-ctype req opt rest)
      ;; Maybe should warn about this stuff too.
      (when (some #'bot-p req)
        (return-from parse-values-ctype (values-bot)))
      (let ((m (member-if #'bot-p opt)))
        (when m
          (return-from parse-values-ctype (cvalues req (ldiff opt m) (bot)))))
      (cvalues req opt rest))))

(defun satisfies-ctype (fname)
  (unless (symbolp fname)
    (error "Bad function name for ~a type: ~a" 'satisfies fname))
  (csatisfies fname))

(defgeneric symbol-specifier-ctype (sym env))
;;; We include all standard CL atomic type specifiers that either can be not
;;; classes (e.g. simple-bit-vector, nil), or which are or can be classes
;;; but which we would prefer a ctype for, like CONS.
(macrolet ((def (sym &body body)
             `(defmethod symbol-specifier-ctype ((sym (eql ',sym)) env)
                (declare (ignore sym) (ignorable env))
                ,@body)))
  (def array (array-ctype :either '* '* env))
  (def atom (negate (ccons (top) (top))))
  (def base-char (charset +base-charset+))
  (def base-string (array-ctype :either 'base-char '(*) env))
  (def bignum (disjunction
               (range 'integer nil nil (1- most-negative-fixnum) nil)
               (range 'integer (1+ most-positive-fixnum) nil nil nil)))
  (def bit (range-ctype 'integer 0 1 env))
  (def bit-vector (array-ctype :either 'bit '(*) env))
  (def boolean (cmember nil t))
  (def character (charset `((0 . ,(1- char-code-limit)))))
  (def compiled-function
      (conjunction (function-top) (csatisfies 'compiled-function-p)))
  (def complex (complex-ctype '* env))
  (def cons (ccons (top) (top)))
  (def double-float (range-ctype 'double-float '* '* env))
  (def extended-char (conjoin (negate (charset +base-charset+))
                              (charset `((0 . ,(1- char-code-limit))))))
  (def fixnum
      (range-ctype 'integer most-negative-fixnum most-positive-fixnum env))
  (def float (range-ctype 'float '* '* env))
  (def function (function-top))
  (def integer (range-ctype 'integer '* '* env))
  (def keyword (conjunction (cclass (find-class 'symbol t env))
                            (csatisfies 'keywordp)))
  (def list (disjunction (cmember nil) (ccons (top) (top))))
  (def long-float (range-ctype 'long-float '* '* env))
  (def nil (bot))
  (def null (cmember nil))
  (def number (disjoin (range-ctype 'real '* '* env)
                       (complex-ctype '* env)))
  (def ratio (range 'ratio nil nil nil nil))
  (def rational (range-ctype 'rational '* '* env))
  (def real (range-ctype 'real '* '* env))
  ;; SEQUENCE is handled specially as a cclass.
  (def short-float (range-ctype 'short-float '* '* env))
  (def signed-byte (range-ctype 'integer '* '* env))
  (def simple-array (array-ctype :simple '* '* env))
  (def simple-base-string (array-ctype :simple 'base-char '(*) env))
  (def simple-bit-vector (array-ctype :simple 'bit '(*) env))
  (def simple-string
      (apply #'disjunction
             (loop for uaet in +string-uaets+
                   collect (array-ctype :simple uaet '(*) env))))
  (def simple-vector (array-ctype :simple 't '(*) env))
  (def single-float (range-ctype 'single-float '* '* env))
  (def standard-char (charset +standard-charset+))
  (def string
      (apply #'disjoin
             (loop for uaet in +string-uaets+
                   collect (array-ctype :either uaet '(*) env))))
  (def t (top))
  (def unsigned-byte (range-ctype 'integer 0 '* env))
  (def vector (array-ctype :either '* '(*) env)))

(defmethod symbol-specifier-ctype ((sym symbol) env)
  (let ((p (assoc sym +class-aliases+)))
    (if p
        (specifier-ctype (second p) env)
        nil)))

(defun class-specifier-ctype (class env)
  (declare (ignore env))
  (cclass class))

(defgeneric cons-specifier-ctype (head rest env))

(defun mapcar-rcurry (arguments function &rest lists)
  (apply #'mapcar
         (lambda (&rest args)
           (multiple-value-call function (values-list args) (values-list arguments)))
         lists))

(macrolet ((def ((head) &body body)
             `(defmethod cons-specifier-ctype ((head (eql ',head)) rest env)
                (declare (ignorable head rest env))
                ,@body)))
  (def (and)
      (apply #'conjoin (mapcar-rcurry (list env) #'specifier-ctype rest)))
  (def (array)
      (destructuring-bind (&optional (et '*) (dims '*)) rest
        (array-ctype :either et dims env)))
  (def (base-string)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype :either 'base-char (list length) env)))
  (def (bit-vector)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype :either 'bit (list length) env)))
  (def (complex)
      (destructuring-bind (&optional (et '*)) rest
        (complex-ctype et env)))
  (def (cons)
      (destructuring-bind (&optional (car '*) (cdr '*)) rest
        (cons-ctype car cdr env)))
  (def (double-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'double-float low high env)))
  (def (eql)
      (destructuring-bind (object) rest (member-ctype (list object))))
  (def (float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'float low high env)))
  (def (function)
      (destructuring-bind (&optional (ll '*) (rv '*)) rest
        (function-ctype ll rv env)))
  (def (integer)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'integer low high env)))
  (def (long-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'long-float low high env)))
  (def (member) (member-ctype rest))
  (def (mod)
      (destructuring-bind (n) rest
        (range-ctype 'integer 0 (list n) env)))
  (def (not)
      (destructuring-bind (spec) rest (negate (specifier-ctype spec env))))
  (def (or) (apply #'disjoin (mapcar-rcurry (list env) #'specifier-ctype rest)))
  (def (rational)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'rational low high env)))
  (def (real)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'real low high env)))
  (def (satisfies)
      (destructuring-bind (fname) rest
        (satisfies-ctype fname)))
  (def (short-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'short-float low high env)))
  (def (signed-byte)
      (destructuring-bind (&optional (s '*)) rest
        (if (eq s '*)
            (range-ctype 'integer '* '* env)
            (let ((es (expt 2 (1- s))))
              (range-ctype 'integer (- es) (1- es) env)))))
  (def (simple-array)
      (destructuring-bind (&optional (et '*) (dims '*)) rest
        (array-ctype :simple et dims env)))
  (def (simple-base-string)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype :simple 'base-char (list length) env)))
  (def (simple-bit-vector)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype :simple 'bit (list length) env)))
  (def (simple-string)
      (destructuring-bind (&optional (length '*)) rest
        (apply #'disjunction
               (loop with l = (list length)
                     for uaet in +string-uaets+
                     collect (array-ctype :simple uaet l env)))))
  (def (simple-vector)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype :simple 't (list length) env)))
  (def (single-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype 'single-float low high env)))
  (def (string)
      (destructuring-bind (&optional (length '*)) rest
        (apply #'disjoin
               (loop with l = (list length)
                     for uaet in +string-uaets+
                     collect (array-ctype :either uaet l env)))))
  (def (unsigned-byte)
      (destructuring-bind (&optional (s '*)) rest
        (range-ctype 'integer 0
                     (if (eq s '*)
                         '*
                         (1- (expt 2 s)))
                     env)))
  (def (values)
      (parse-values-ctype rest env))
  (def (vector)
      (destructuring-bind (&optional (et '*) (length '*)) rest
        (array-ctype :either et (list length) env))))

(defvar *parse-extended-types* nil
  "When `t', `specifier-ctype' will parse extended types. Use
  `extended-specifier-ctype' instead of using this variable directly.")

(defun remove-environment (lambda-list)
  "Return(0) a new lambda list like LAMBDA-LIST but without the &environment
  parameter. Return(1) the name of the removed &environment parameter."
  (let (environment)
    (values
     (loop for keys on lambda-list
           for key = (first keys)
           if (eq key '&environment) do
             (setq environment (second keys)
                   keys (rest keys))
           else collect key)
     environment)))

(defmacro define-extended-type (name lambda-list &key (documentation "") simple extended)
  "Define a type NAME that can be used as a type specifier and as a constructor
  for a custom ctype. The :simple expander is used by programs that only work
  with type specifiers like `specifier-ctype'. The :extended expander is used by
  programs that can take advantage of ctype extensions like
  `extended-specifier-ctype'.

  SIMPLE is a list of forms that return a type specifier that might not
  completely represent the custom type.

  EXTENDED is a list of forms that return a ctype that completely represents the
  custom type.

  Both the SIMPLE and the EXTENDED forms share the parameters of LAMBDA-LIST.

  LAMBDA-LIST is a macro lambda list."
  (assert simple nil "simple form is required")
  (assert extended nil "extended form is required")
  `(progn
     (deftype ,name ,lambda-list
       ,documentation
       ,@simple)
     (setf (get ',name 'extended-type-parser)
           ,(multiple-value-bind
                  (clean-lambda-list env-name) (remove-environment lambda-list)
              (let* ((args (gensym)) (env (gensym))
                     (body `(destructuring-bind ,clean-lambda-list ,args
                              ,@extended)))
                `(lambda (,args ,env)
                   ,documentation
                   ,@(if env-name
                         `((let ((,env-name ,env))
                            ,body))
                         `((declare (ignore ,env))
                           ,body))))))
     ',name))

(defun parse (specifier env)
  (flet ((parse-symbol (spec)
           (or (symbol-specifier-ctype spec env)
               (class-specifier-ctype (find-class spec t env) env)))
         (parse-class (spec)
           (or (symbol-specifier-ctype (class-name spec) env))))
    (if *parse-extended-types*
        (etypecase specifier
          (cons (let* ((name (car specifier))
                       (args (cdr specifier))
                       (parser (get name 'extended-type-parser)))
                  (if parser
                      (let ((*env* env))
                        (apply parser args))
                      (cons-specifier-ctype name args env))))
          (symbol (let ((parser (get specifier 'extended-type-parser)))
                    (if parser
                        (let ((*env* env))
                          (funcall parser))
                        (parse-symbol specifier))))
          (class (parse-class specifier)))
        (let ((spec (typexpand specifier env)))
          (etypecase spec
            (cons (cons-specifier-ctype (car spec) (cdr spec) env))
            (symbol (parse-symbol spec))
            (class (parse-class spec)))))))

(defun specifier-ctype (specifier &optional env)
  (let ((ct (parse specifier env)))
    (when (typep ct 'cvalues)
      (error "Found ~s in non-~s context" (unparse ct) 'values))
    ct))

(defun values-specifier-ctype (specifier &optional env)
  (let ((ct (parse specifier env)))
    (if (typep ct 'cvalues)
        ct
        ;; Treat X as (values X).
        (parse-values-ctype `(,specifier) env))))

(defun extended-specifier-ctype (specifier &optional env)
  "Return the ctype specified by the possibly extended SPECIFIER."
  (let ((*parse-extended-types* t))
    (specifier-ctype specifier env)))

(defun extended-values-specifier-ctype (specifier &optional env)
  "Return the ctype specified by the possibly extended values SPECIFIER."
  (let ((*parse-extended-types* t))
    (values-specifier-ctype specifier env)))
