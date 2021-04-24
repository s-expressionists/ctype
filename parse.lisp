(in-package #:ctype)

(defun array-ctype (simplicity et dims env)
  (let ((uaet (if (eq et '*)
                  et
                  (upgraded-array-element-type et env)))
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
            (disjunction (carray :simple uaet dims) (carray :complex uaet dims))
            (carray simplicity uaet dims))
        (carray :simple uaet dims))))

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

(defun function-ctype (ll rv env)
  (let ((ll (if (eq ll '*)
                (make-instance 'lambda-list
                  :required nil :optional nil :rest (top)
                  :keyp nil :keys nil :aokp nil)
                (parse-lambda-list ll env)))
        (rv (if (eq rv '*)
                (top)
                (specifier-ctype rv))))
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

(defun parse-values-ctype (rest env)
  (multiple-value-bind (req opt rest) (%parse-values-ctype rest env)
    ;; Maybe should warn about this stuff too.
    (when (some #'bot-p req) (return-from parse-values-ctype (bot)))
    (let ((m (member-if #'bot-p opt)))
      (when m
        (return-from parse-values-ctype (cvalues req (ldiff opt m) (bot)))))
    ;; (top) because of CL:THE fuzziness.
    (cvalues req opt (or rest (top)))))

(defun satisfies-ctype (fname)
  (unless (symbolp fname)
    (error "Bad function name for ~a type: ~a" 'satisfies fname))
  (csatisfies fname))

(defun symbol-specifier-ctype (sym &optional env)
  (case sym
    ;; We include all standard CL atomic type specifiers that either can be not
    ;; classes (e.g. simple-bit-vector, nil), or which are or can be classes
    ;; but which we would prefer a ctype for, like CONS.
    ((array) (array-ctype :either '* '* env))
    ((atom) (negate (ccons (top) (top))))
    ((base-char) (charset +base-charset+))
    ((base-string) (array-ctype :either 'base-char '(*) env))
    ((bignum) (disjunction
               (range 'integer nil nil (1- most-negative-fixnum) nil)
               (range 'integer (1+ most-positive-fixnum) nil nil nil)))
    ((bit-vector) (array-ctype :either 'bit '(*) env))
    ((boolean) (cmember nil t))
    ((character) (charset `((0 . ,(1- char-code-limit)))))
    ((compiled-function)
     (conjunction (function-ctype '* '* env) (csatisfies 'compiled-function-p)))
    ((complex) (complex-ctype '* env))
    ((cons) (ccons (top) (top)))
    ((double-float) (range-ctype 'double-float '* '* env))
    ((extended-char) (negate (charset +base-charset+)))
    ((fixnum)
     (range-ctype 'integer most-negative-fixnum most-positive-fixnum env))
    ((float) (range-ctype 'float '* '* env))
    ((function) (function-ctype '* '* env))
    ((integer) (range-ctype 'integer '* '* env))
    ((keyword) (conjunction (cclass (find-class 'symbol t env))
                            (csatisfies 'keywordp)))
    ((list) (disjunction (cmember nil) (ccons (top) (top))))
    ((long-float) (range-ctype 'long-float '* '* env))
    ((nil) (bot))
    ((null) (cmember nil))
    ((number) (disjoin (range-ctype 'real '* '* env)
                       (complex-ctype '* env)))
    ((ratio) (range 'ratio nil nil nil nil))
    ((rational) (range-ctype 'rational '* '* env))
    ((real) (range-ctype 'real '* '* env))
    ;; SEQUENCE is handled specially as a cclass.
    ((short-float) (range-ctype 'short-float '* '* env))
    ((signed-byte) (range-ctype 'integer '* '* env))
    ((simple-array) (array-ctype :simple '* '* env))
    ((simple-base-string) (array-ctype :simple 'base-char '(*) env))
    ((simple-bit-vector) (array-ctype :simple 'bit '(*) env))
    ((simple-string)
     (apply #'disjunction
            (loop for uaet in +string-uaets+
                  collect (array-ctype :simple uaet '(*) env))))
    ((simple-vector) (array-ctype :simple 't '(*) env))
    ((single-float) (range-ctype 'single-float '* '* env))
    ((standard-char) (charset +standard-charset+))
    ((string)
     (apply #'disjoin
            (loop for uaet in +string-uaets+
                  collect (array-ctype :either uaet '(*) env))))
    ((t) (top))
    ((unsigned-byte) (range-ctype 'integer 0 '* env))
    ((vector) (array-ctype :either '* '(*) env))
    (otherwise
     (let ((p (assoc sym +class-aliases+)))
       (when p
         (specifier-ctype (second p) env))))))

(defun class-specifier-ctype (class env)
  (declare (ignore env))
  (cclass class))

(defun cons-specifier-ctype (head rest env)
  (flet ((recur (spec) (specifier-ctype spec env)))
    (ecase head
      ((and) (apply #'conjoin (mapcar #'recur rest)))
      ((array) (destructuring-bind (&optional (et '*) (dims '*)) rest
                 (array-ctype :either et dims env)))
      ((base-string) (destructuring-bind (&optional (length '*)) rest
                       (array-ctype :either 'base-char (list length) env)))
      ((bit-vector) (destructuring-bind (&optional (length '*)) rest
                      (array-ctype :either 'bit (list length) env)))
      ((complex) (destructuring-bind (&optional (et '*)) rest
                   (complex-ctype et env)))
      ((cons) (destructuring-bind (&optional (car '*) (cdr '*)) rest
                (cons-ctype car cdr env)))
      ((double-float) (destructuring-bind (&optional (low '*) (high '*)) rest
                        (range-ctype 'double-float low high env)))
      ((eql) (destructuring-bind (object) rest (member-ctype (list object))))
      ((float) (destructuring-bind (&optional (low '*) (high '*)) rest
                 (range-ctype 'float low high env)))
      ((function) (destructuring-bind (&optional (ll '*) (rv '*)) rest
                    (function-ctype ll rv env)))
      ((integer) (destructuring-bind (&optional (low '*) (high '*)) rest
                   (range-ctype 'integer low high env)))
      ((long-float) (destructuring-bind (&optional (low '*) (high '*)) rest
                      (range-ctype 'long-float low high env)))
      ((member) (member-ctype rest))
      ((mod) (destructuring-bind (n) rest
               (range-ctype 'integer 0 (list n) env)))
      ((not) (destructuring-bind (spec) rest (negate (recur spec))))
      ((or) (apply #'disjoin (mapcar #'recur rest)))
      ((rational) (destructuring-bind (&optional (low '*) (high '*)) rest
                    (range-ctype 'rational low high env)))
      ((real) (destructuring-bind (&optional (low '*) (high '*)) rest
                (range-ctype 'real low high env)))
      ((satisfies) (destructuring-bind (fname) rest
                     (satisfies-ctype fname)))
      ((short-float) (destructuring-bind (&optional (low '*) (high '*)) rest
                       (range-ctype 'short-float low high env)))
      ((signed-byte) (destructuring-bind (&optional (s '*)) rest
                       (if (eq s '*)
                           (range-ctype 'integer '* '* env)
                           (let ((es (expt 2 (1- s))))
                             (range-ctype 'integer (- es) (1- es) env)))))
      ((simple-array) (destructuring-bind (&optional (et '*) (dims '*)) rest
                        (array-ctype :simple et dims env)))
      ((simple-base-string)
       (destructuring-bind (&optional (length '*)) rest
         (array-ctype :simple 'base-char (list length) env)))
      ((simple-bit-vector) (destructuring-bind (&optional (length '*)) rest
                             (array-ctype :simple 'bit (list length) env)))
      ((simple-string)
       (destructuring-bind (&optional (length '*)) rest
         (apply #'disjunction
                (loop with l = (list length)
                      for uaet in +string-uaets+
                      collect (array-ctype :simple uaet l env)))))
      ((simple-vector) (destructuring-bind (&optional (length '*)) rest
                         (array-ctype :simple 't (list length) env)))
      ((single-float) (destructuring-bind (&optional (low '*) (high '*)) rest
                        (range-ctype 'single-float low high env)))
      ((string) (destructuring-bind (&optional (length '*)) rest
                  (apply #'disjoin
                         (loop with l = (list length)
                               for uaet in +string-uaets+
                               collect (array-ctype :either uaet l env)))))
      ((unsigned-byte) (destructuring-bind (&optional (s '*)) rest
                         (range-ctype 'integer 0
                                      (if (eq s '*)
                                          '*
                                          (1- (expt 2 s)))
                                      env)))
      ((values) (parse-values-ctype rest env))
      ((vector) (destructuring-bind (&optional (et '*) (length '*)) rest
                  (array-ctype :either et (list length) env))))))

(defun specifier-ctype (specifier &optional env)
  (let ((spec (typexpand specifier env)))
    (etypecase spec
      (cons (cons-specifier-ctype (car spec) (cdr spec) env))
      (symbol (or (symbol-specifier-ctype spec env)
                  (class-specifier-ctype (find-class specifier t env) env)))
      (class (or (symbol-specifier-ctype (class-name spec) env)
                 (class-specifier-ctype spec env))))))
