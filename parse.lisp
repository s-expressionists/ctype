(in-package #:ctype)

(defun array-ctype (client simplicity et dims env)
  (let ((uaet (if (eq et '*)
                  et
                  (upgraded-array-element-type client et env)))
        (eaet (if (eq et '*) (top) (specifier-ctype client et env)))
        (dims (cond ((eq dims '*) dims)
                    ((and (integerp dims) (>= dims 0))
                     (make-list dims :initial-element '*))
                    ((and (listp dims)
                          (every (lambda (d) (or (and (integerp d) (>= d 0))
                                                 (eq d '*)))
                                 dims))
                     dims)
                    (t (error "Invalid dimension specification: ~a" dims)))))
    (if (complex-arrays-distinct-p client)
        (if (eq simplicity :either)
            (disjunction (carray :simple uaet eaet dims)
                         (carray :complex uaet eaet dims))
            (carray simplicity uaet eaet dims))
        (carray :simple uaet eaet dims))))

(defun cons-ctype (client car cdr env)
  (let ((car (if (eq car '*)
                 (top)
                 (specifier-ctype client car env)))
        (cdr (if (eq cdr '*)
                 (top)
                 (specifier-ctype client cdr env))))
    (ccons car cdr)))

(defun member-ctype (client elements)
  ;; Cut out real ranges, floating point zeroes, and character sets.
  (apply
   #'disjoin
   client
   (loop for elem in elements
         collect (cond ((integerp elem) (range 'integer elem nil elem nil))
                       ((range-kindp client elem 'ratio)
                        (range 'ratio elem nil elem nil))
                       ((characterp elem)
                        (let ((code (char-code elem)))
                          (charset (list (cons code code)))))
                       ((floatp elem)
                        (loop for type in (distinct-float-types client)
                              when (range-kindp client elem type)
                                return (if (and (zerop elem)
                                                (distinct-zeroes-p client type))
                                           (fpzero type elem)
                                           (range type elem nil elem nil))
                              finally (error "BUG in float type specialization")))
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

(defun rational-range (client low lxp high hxp)
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
        (disjoin client
                 (range 'integer ilow nil ihigh nil)
                 (range 'ratio low lxp high hxp)))))

(defun float-range (client low lxp high hxp)
  (let ((lj (loop for (ty) in (distinct-float-types client)
                  for nl = (if low (coerce low ty) low)
                  for nh = (if high (coerce high ty) high)
                  collect (range ty nl lxp nh hxp))))
    (apply #'disjoin client lj)))

(defun range-ctype (client kind low high env)
  (declare (ignore env))
  (let* ((rkind (if (member kind
                            '(short-float single-float double-float long-float))
                    (reduce-float-type client kind)
                    kind))
         (test (ecase rkind
                 ((integer) #'integerp)
                 ((rational) #'rationalp)
                 ((float) #'floatp)
                 ((real) #'realp)
                 ((ratio short-float single-float double-float long-float)
                  (lambda (o) (range-kindp client o kind))))))
    (multiple-value-bind (nlow lxp) (parse-interval-designator low)
      (multiple-value-bind (nhigh hxp) (parse-interval-designator high)
        (unless (or (not nlow) (funcall test nlow))
          (error-interval-designator low kind))
        (unless (or (not nhigh) (funcall test nhigh))
          (error-interval-designator high kind))
        (ecase rkind
          ((integer) (range 'integer nlow lxp nhigh hxp))
          ((rational) (rational-range client nlow lxp nhigh hxp))
          ((float) (float-range client nlow lxp nhigh hxp))
          ((real) (disjoin client
                           (float-range client nlow lxp nhigh hxp)
                           (rational-range client nlow lxp nhigh hxp)))
          ((short-float single-float double-float long-float)
           (range rkind nlow lxp nhigh hxp)))))))

(defun complex-ctype (client element-type env)
  (ccomplex (if (eq element-type '*)
                element-type
                (upgraded-complex-part-type client element-type env))))

(defun %parse-lambda-list (client ll env)
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
                  ((:required) (push (specifier-ctype client elem env) req))
                  ((&optional) (push (specifier-ctype client elem env) opt))
                  ((&rest)
                   (when rest (error "Bad syntax in lambda-list ~a" ll))
                   (setf rest (specifier-ctype client elem env)))
                  ((&key)
                   (destructuring-bind (keyword spec) elem
                     (unless (symbolp keyword)
                       (error "Bad syntax in lambda-list ~a" ll))
                     (push (cons keyword (specifier-ctype client spec env)) key)))
                  ((&allow-other-keys)
                   (error "Bad syntax in lambda-list ~a" ll)))))
        finally (return (values (nreverse req) (nreverse opt)
                                rest keyp key aokp))))

(defun parse-lambda-list (client ll env)
  (multiple-value-bind (req opt rest keyp key aokp)
      (%parse-lambda-list client ll env)
    ;; I was going to have some checks of whether any of the types are bot,
    ;; but probably that merits a warning... or osmething... instead?
    (make-instance 'lambda-list
      :required req :optional opt :rest (or rest (bot))
      :keyp keyp :keys key :aokp aokp)))

(defgeneric coerce-to-values (ctype))
(defmethod coerce-to-values ((ctype cvalues)) ctype)
(defmethod coerce-to-values ((ctype ctype))
  (cvalues (list ctype) nil (top)))

(defun function-ctype (client ll rv env)
  (let ((ll (if (eq ll '*)
                (make-instance 'lambda-list
                  :required nil :optional nil :rest (top)
                  :keyp nil :keys nil :aokp nil)
                (parse-lambda-list client ll env)))
        (rv (if (eq rv '*)
                (cvalues nil nil (top))
                (values-specifier-ctype client rv env))))
    (if (bot-p ll)
        ll
        (make-instance 'cfunction :lambda-list ll :returns rv))))

(defun %parse-values-ctype (client vest env)
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
                  ((:required) (push (specifier-ctype client elem env) req))
                  ((&optional) (push (specifier-ctype client elem env) opt))
                  ((&rest)
                   (when rest (fail))
                   (setf rest (specifier-ctype client elem env))))))
          finally (return (values (nreverse req) (nreverse opt) rest)))))

(defun %fuzz-values-ctype (client required optional rest)
  ;; CTYPE internally treats VALUES types with the strict semantics described
  ;; in the entry on the VALUES type. However, these semantics are not used in
  ;; any actual place in the language, and in particular, when used in THE
  ;; VALUES types are considerably vaguer. This function applies that vagueness:
  ;; (1) if &rest is not declared, &rest t is implicit
  ;; (2) if a suffix of the "required" types includes NULL, those values are
  ;;     not actually required.
  ;; If you want strict semantics, just make a CVALUES directly.
  (let* ((rest (or rest (top)))
         (rpos (position-if-not (lambda (ct) (ctypep client nil ct))
                                required :from-end t))
         (rrpos (if rpos (1+ rpos) 0))
         (rreq (subseq required 0 rrpos))
         (ropt (append (nthcdr rrpos required) optional)))
    (values rreq ropt rest)))

(defun parse-values-ctype (client rest env)
  (multiple-value-bind (req opt rest) (%parse-values-ctype client rest env)
    (multiple-value-bind (req opt rest) (%fuzz-values-ctype client req opt rest)
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

(defgeneric symbol-specifier-ctype (client sym env))
;;; We include all standard CL atomic type specifiers that either can be not
;;; classes (e.g. simple-bit-vector, nil), or which are or can be classes
;;; but which we would prefer a ctype for, like CONS.
(macrolet ((def (sym &body body)
             `(defmethod symbol-specifier-ctype (client (sym (eql ',sym)) env)
                (declare (ignore sym) (ignorable client env))
                ,@body)))
  (def array (array-ctype client :either '* '* env))
  (def atom (negate client (ccons (top) (top))))
  (def base-char (charset (base-charset-pairs client)))
  (def base-string (array-ctype client :either 'base-char '(*) env))
  (def bignum (disjunction
               (range 'integer nil nil (1- (most-negative-fixnum client)) nil)
               (range 'integer (1+ (most-positive-fixnum client)) nil nil nil)))
  (def bit (range-ctype client 'integer 0 1 env))
  (def bit-vector (array-ctype client :either 'bit '(*) env))
  (def boolean (cmember nil t))
  (def character (charset `((0 . ,(1- (char-code-limit client))))))
  (def compiled-function
      (conjunction (function-top) (csatisfies 'compiled-function-p)))
  (def complex (complex-ctype client '* env))
  (def cons (ccons (top) (top)))
  (def double-float (range-ctype client 'double-float '* '* env))
  (def extended-char (conjoin client
                              (negate client (charset (base-charset-pairs client)))
                              (charset `((0 . ,(1- (char-code-limit client)))))))
  (def fixnum (range 'integer (most-negative-fixnum client) nil
                     (most-positive-fixnum client) nil))
  (def float (range-ctype client 'float '* '* env))
  (def function (function-top))
  (def integer (range-ctype client 'integer '* '* env))
  (def keyword (conjunction (cclass (find-class client 'symbol t env))
                            (csatisfies 'keywordp)))
  (def list (disjunction (cmember nil) (ccons (top) (top))))
  (def long-float (range-ctype client 'long-float '* '* env))
  (def nil (bot))
  (def null (cmember nil))
  (def number (disjoin client
                       (range-ctype client 'real '* '* env)
                       (complex-ctype client '* env)))
  (def ratio (range 'ratio nil nil nil nil))
  (def rational (range-ctype client 'rational '* '* env))
  (def real (range-ctype client 'real '* '* env))
  ;; SEQUENCE is handled specially as a cclass.
  (def short-float (range-ctype client 'short-float '* '* env))
  (def signed-byte (range-ctype client 'integer '* '* env))
  (def simple-array (array-ctype client :simple '* '* env))
  (def simple-base-string (array-ctype client :simple 'base-char '(*) env))
  (def simple-bit-vector (array-ctype client :simple 'bit '(*) env))
  (def simple-string
      (apply #'disjunction
             (loop for uaet in (string-uaets client)
                   collect (array-ctype client :simple uaet '(*) env))))
  (def simple-vector (array-ctype client :simple 't '(*) env))
  (def single-float (range-ctype client 'single-float '* '* env))
  (def standard-char (charset (standard-charset-pairs client)))
  (def string
      (apply #'disjoin
             (loop for uaet in (string-uaets client)
                   collect (array-ctype client :either uaet '(*) env))))
  (def t (top))
  (def unsigned-byte (range-ctype client 'integer 0 '* env))
  (def vector (array-ctype client :either '* '(*) env)))

(defmethod symbol-specifier-ctype (client (sym symbol) env)
  (let ((p (class-alias client sym)))
    (if p
        (specifier-ctype client p env)
        nil)))

(defun class-specifier-ctype (class env)
  (declare (ignore env))
  (cclass class))

(defgeneric cons-specifier-ctype (client head rest env))

(macrolet ((def ((head) &body body)
             `(defmethod cons-specifier-ctype (client (head (eql ',head)) rest env)
                (declare (ignorable head client rest env))
                ,@body)))
  (def (and)
      (apply #'conjoin client
             (mapcar (lambda (spec) (specifier-ctype client spec env)) rest)))
  (def (array)
      (destructuring-bind (&optional (et '*) (dims '*)) rest
        (array-ctype client :either et dims env)))
  (def (base-string)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype client :either 'base-char (list length) env)))
  (def (bit-vector)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype client :either 'bit (list length) env)))
  (def (complex)
      (destructuring-bind (&optional (et '*)) rest
        (complex-ctype client et env)))
  (def (cons)
      (destructuring-bind (&optional (car '*) (cdr '*)) rest
        (cons-ctype client car cdr env)))
  (def (double-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'double-float low high env)))
  (def (eql)
      (destructuring-bind (object) rest (member-ctype client (list object))))
  (def (float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'float low high env)))
  (def (function)
      (destructuring-bind (&optional (ll '*) (rv '*)) rest
        (function-ctype client ll rv env)))
  (def (integer)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'integer low high env)))
  (def (long-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'long-float low high env)))
  (def (member) (member-ctype client rest))
  (def (mod)
      (destructuring-bind (n) rest
        (range-ctype client 'integer 0 (list n) env)))
  (def (not)
      (destructuring-bind (spec) rest
        (negate client (specifier-ctype client spec env))))
  (def (or)
      (apply #'disjoin client
             (mapcar (lambda (spec) (specifier-ctype client spec env)) rest)))
  (def (rational)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'rational low high env)))
  (def (real)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'real low high env)))
  (def (satisfies)
      (destructuring-bind (fname) rest
        (satisfies-ctype fname)))
  (def (short-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'short-float low high env)))
  (def (signed-byte)
      (destructuring-bind (&optional (s '*)) rest
        (if (eq s '*)
            (range-ctype client 'integer '* '* env)
            (let ((es (expt 2 (1- s))))
              (range-ctype client 'integer (- es) (1- es) env)))))
  (def (simple-array)
      (destructuring-bind (&optional (et '*) (dims '*)) rest
        (array-ctype client :simple et dims env)))
  (def (simple-base-string)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype client :simple 'base-char (list length) env)))
  (def (simple-bit-vector)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype client :simple 'bit (list length) env)))
  (def (simple-string)
      (destructuring-bind (&optional (length '*)) rest
        (apply #'disjunction
               (loop with l = (list length)
                     for uaet in (string-uaets client)
                     collect (array-ctype client :simple uaet l env)))))
  (def (simple-vector)
      (destructuring-bind (&optional (length '*)) rest
        (array-ctype client :simple 't (list length) env)))
  (def (single-float)
      (destructuring-bind (&optional (low '*) (high '*)) rest
        (range-ctype client 'single-float low high env)))
  (def (string)
      (destructuring-bind (&optional (length '*)) rest
        (apply #'disjoin client
               (loop with l = (list length)
                     for uaet in (string-uaets client)
                     collect (array-ctype client :either uaet l env)))))
  (def (unsigned-byte)
      (destructuring-bind (&optional (s '*)) rest
        (range-ctype client 'integer 0
                     (if (eq s '*)
                         '*
                         (1- (expt 2 s)))
                     env)))
  (def (values)
      (parse-values-ctype client rest env))
  (def (vector)
      (destructuring-bind (&optional (et '*) (length '*)) rest
        (array-ctype client :either et (list length) env))))

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

(defun declare-p (form)
  (and (consp form)
       (eq 'declare (first form))))

(defun declaration-abouts (declaration)
  (case (first declaration)
    ((type ftype) (rest (rest declaration)))
    (otherwise (rest declaration))))

(defun declaration-specifier (declaration)
  (case (first declaration)
    ((type ftype) (subseq declaration 0 2))
    (otherwise (subseq declaration 0 1))))

(defun declarations-by (filter forms)
  (let (result)
    (dolist (form forms result)
      (if (not (declare-p form))
          (push form result)
          (dolist (declaration (rest form))
            (let ((vars (funcall filter (declaration-abouts declaration))))
              (when vars
                (push (list 'declare
                            (append (declaration-specifier declaration)
                                    vars))
                      result))))))
    (nreverse result)))

(defun remove-declarations-for (var-name forms)
  (declarations-by
   (lambda (abouts)
     (remove var-name abouts))
   forms))

(defun declarations-for (var-name forms)
  (remove-if-not
   #'declare-p
   (declarations-by
    (lambda (abouts)
      (remove var-name abouts :test-not #'eql))
    forms)))

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
                              ,@(if env-name
                                    (remove-declarations-for env-name extended)
                                    extended))))
                `(lambda (,args ,env)
                   ,documentation
                   ,@(if env-name
                         `((let ((,env-name ,env))
                             ,@(declarations-for env-name extended)
                            ,body))
                         `((declare (ignore ,env))
                           ,body))))))
     ',name))

(defun parse (client specifier env)
  (flet ((regular-parse ()
           (let ((spec (typexpand client specifier env)))
             (etypecase spec
               (cons (cons-specifier-ctype client (car spec) (cdr spec) env))
               (symbol (or (symbol-specifier-ctype client spec env)
                           (class-specifier-ctype
                            (find-class client spec t env) env)))
               (class (symbol-specifier-ctype client (class-name spec) env))))))
    (if *parse-extended-types*
        (typecase specifier
          (cons (let* ((name (car specifier))
                       (args (cdr specifier))
                       (parser (get name 'extended-type-parser)))
                  (if parser
                      (funcall parser args env)
                      (regular-parse))))
          (symbol (let ((parser (get specifier 'extended-type-parser)))
                    (if parser
                        (funcall parser nil env)
                        (regular-parse))))
          (otherwise (regular-parse)))
        (regular-parse))))

(defun specifier-ctype (client specifier &optional env)
  (let ((ct (parse client specifier env)))
    (when (typep ct 'cvalues)
      (error "Found ~s in non-~s context" (unparse ct) 'values))
    (setf (%specifier ct) specifier)
    ct))

(defun values-specifier-ctype (client specifier &optional env)
  (let ((ct (parse client specifier env)))
    (setf (%specifier ct) specifier)
    (if (typep ct 'cvalues)
        ct
        ;; Treat X as (values X).
        (parse-values-ctype client `(,specifier) env))))

(defun extended-specifier-ctype (client specifier &optional env)
  "Return the ctype specified by the possibly extended SPECIFIER."
  (let ((*parse-extended-types* t))
    (specifier-ctype client specifier env)))

(defun extended-values-specifier-ctype (client specifier &optional env)
  "Return the ctype specified by the possibly extended values SPECIFIER."
  (let ((*parse-extended-types* t))
    (values-specifier-ctype client specifier env)))
