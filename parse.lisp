(in-package #:ctype)

(defun array-ctype (et dims env)
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
    (make-instance 'carray :uaet uaet :dims dims)))

(defun cons-ctype (car cdr env)
  (let ((car (if (eq car '*)
                 (top)
                 (specifier-ctype car env)))
        (cdr (if (eq cdr '*)
                 (top)
                 (specifier-ctype cdr env))))
    (ccons car cdr)))

(defun member-ctype (elements)
  ;; Cut out integer ranges.
  ;; Doing real ranges more generally might be a good idea (TODO?),
  ;; but those are not as mergeable.
  (multiple-value-bind (integers non)
      (loop for elem in elements
            if (integerp elem)
              collect elem into integers
            else collect elem into non
            finally (return (values integers non)))
    (if integers
        (apply #'disjoin ; let disjunction handle merging
               (apply #'cmember non)
               (loop for i in integers
                     collect (make-instance 'range
                               :kind 'integer
                               :low i :lxp nil :high i :hxp nil)))
        (apply #'cmember non))))

(defun error-interval-designator (nondesignator &optional kind)
  (error "~a is not a valid interval designator~@[ for type ~a~}"
         nondesignator kind))

(defun parse-interval-designator (designator)
  (cond ((eq designator '*) (values nil nil))
        ((realp designator) (values designator nil))
        ((and (consp designator) (null (cdr designator))
              (realp (car designator)))
         (values (car designator) t))
        (t (error-interval-designator designator))))

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
        (when (and nlow nhigh
                   (or (> nlow nhigh)
                       (and (= nlow nhigh)
                            (or lxp hxp))))
          (return-from range-ctype (bot)))
        (ecase kind
          ((integer)
           (let ((nlow (if (and nlow lxp) (1+ nlow) nlow))
                 (nhigh (if (and nhigh lxp) (1- nhigh) nhigh)))
             (if (and nlow nhigh (> nlow nhigh))
                 (bot)
                 (make-instance 'range :kind 'integer
                                :low nlow :lxp nil :high nhigh :hxp nil))))
          ((rational)
           ;; Check if this is a ratio-only range like (rational (0) (1))
           (if (and nlow nhigh
                    (or (< nhigh (ceiling nlow))
                        (and hxp
                             (or (= nhigh (ceiling nlow))
                                 (and (integerp nlow)
                                      (= nhigh (1+ nlow))
                                      lxp)))))
               (make-instance 'range :kind 'ratio
                              :low nlow :lxp lxp :high nhigh :hxp hxp)
               (disjunction
                (make-instance 'range :kind 'integer
                               :low nlow :lxp lxp :high nhigh :hxp hxp)
                (make-instance 'range :kind 'ratio
                               :low nlow :lxp lxp :high nhigh :hxp hxp))))
          ((float)
           (let ((lj (loop for (ty) in +floats+
                           for nl = (if nlow
                                        (coerce nlow ty)
                                        nlow)
                           for nh = (if nhigh
                                        (coerce nhigh ty)
                                        nhigh)
                           collect (make-instance 'range
                                     :kind ty
                                     :low nl :lxp lxp :high nh :hxp hxp))))
             (if (null (rest lj))
                 (first lj)
                 (apply #'disjunction lj))))
          ((short-float)
           (make-instance 'range
             :kind (if (assoc 'short-float +floats+)
                       'short-float
                       'single-float)
             :low nlow :lxp lxp :high nhigh :hxp hxp))
          ((single-float)
           (make-instance 'range
             :kind 'single-float :low nlow :lxp lxp :high nhigh :hxp hxp))
          ((double-float)
           (make-instance 'range
             :kind (if (assoc 'double-float +floats+)
                       'double-float
                       'single-float)
             :low nlow :lxp lxp :high nhigh :hxp hxp))
          ((long-float)
           (make-instance 'range
             :kind (cond ((assoc 'long-float +floats+) 'long-float)
                         ((assoc 'double-float +floats+) 'double-float)
                         (t 'single-float))
             :low nlow :lxp lxp :high nhigh :hxp hxp))
          ((real)
           (let ((lj (loop for (ty) in +floats+
                           for nl = (if nlow
                                        (coerce nlow ty)
                                        nlow)
                           for nh = (if nhigh
                                        (coerce nhigh ty)
                                        nhigh)
                           collect (make-instance 'range
                                     :kind ty
                                     :low nl :lxp lxp :high nh :hxp hxp)))
                 (rlow (if low (rational low) nil))
                 (rhigh (if high (rational high) nil)))
             ;; TODO determine if integer range is empty, etc
             (apply #'disjunction
                    (make-instance 'range :kind 'ratio
                                   :low rlow :lxp lxp :high rhigh :hxp hxp)
                    (make-instance 'range :kind 'integer
                                   :low rlow :lxp lxp :high rhigh :hxp hxp)
                    lj))))))))

(defun complex-ctype (element-type env)
  (make-instance 'ccomplex
    :ucpt (if (eq element-type '*)
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
  (make-instance 'csatisfies :fname fname))

(defun symbol-specifier-ctype (sym &optional env)
  (case sym
    ;; We include any CL type that an implementation might have defined as
    ;; a class but which we would like to be a ctype (which is kind of a lot of
    ;; them) except in some subclassing cases, like with subclasses of FUNCTION.
    ((array) (array-ctype '* '* env))
    ((atom) (negate (ccons (top) (top))))
    #+(or)
    ((base-char) ...)
    ((base-string) (array-ctype 'base-char '* env))
    ((bignum) (negate (specifier-ctype 'fixnum env)))
    ((bit-vector) (array-ctype 'bit '(*) env))
    ((compiled-function)
     (conjunction (function-ctype '* '* env)
                  (make-instance 'csatisfies :fname 'compiled-function-p)))
    ((complex) (complex-ctype '* env))
    ((cons) (ccons (top) (top)))
    ((double-float) (range-ctype 'double-float '* '* env))
    ((fixnum)
     (range-ctype 'integer most-negative-fixnum most-positive-fixnum env))
    ((float) (range-ctype 'float '* '* env))
    ((function) (function-ctype '* '* env))
    ((integer) (range-ctype 'integer '* '* env))
    ((keyword) (conjunction (make-instance 'cclass
                              :class (find-class 'symbol t env))
                            (make-instance 'csatisfies :fname 'keywordp)))
    ((list) (disjunction (cmember nil) (ccons (top) (top))))
    ((long-float) (range-ctype 'long-float '* '* env))
    ((null) (cmember nil))
    ((number) (disjunction (range-ctype 'real '* '* env)
                           (complex-ctype '* env)))
    ((ratio) (make-instance 'range
               :kind 'ratio :low nil :lxp nil :high nil :hxp nil))
    ((rational) (range-ctype 'rational '* '* env))
    ((real) (range-ctype 'real '* '* env))
    ;; SEQUENCE is handled specially as a cclass.
    ((short-float) (range-ctype 'short-float '* '* env))
    ((signed-byte) (range-ctype 'integer '* '* env))
    ((simple-array) (array-ctype '* '* env))
    ((simple-base-string) (array-ctype 'base-char '* env))
    ((simple-bit-vector) (array-ctype 'bit '* env))
    #+(or)
    ((simple-string) ...)
    ((simple-vector) (array-ctype 't '(*) env))
    ((single-float) (range-ctype 'single-float '* '* env))
    #+(or)
    ((standard-char) ...)
    #+(or)
    ((string) ...)
    ((t) (top))
    ((unsigned-byte) (range-ctype 'integer 0 '* env))
    ((vector) (array-ctype '* '(*) env))))

(defun class-specifier-ctype (class env)
  (declare (ignore env))
  (make-instance 'cclass :class class))

(defun cons-specifier-ctype (head rest env)
  (flet ((recur (spec) (specifier-ctype spec env)))
    (ecase head
      ((and) (apply #'conjoin (mapcar #'recur rest)))
      ((array) (destructuring-bind (&optional (et '*) (dims '*)) rest
                 (array-ctype et dims)))
      ((base-string) (destructuring-bind (&optional (length '*)) rest
                       (array-ctype 'base-char (list length) env)))
      ((bit-vector) (destructuring-bind (&optional (length '*)) rest
                      (array-ctype 'bit (list length) env)))
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
                        (array-ctype et dims env)))
      ((simple-base-string) (destructuring-bind (&optional (length '*)) rest
                              (array-ctype 'base-char (list length) env)))
      ((simple-bit-vector) (destructuring-bind (&optional (length '*)) rest
                             (array-ctype 'bit (list length) env)))
      #+(or)
      ((simple-string) ...)
      ((simple-vector) (destructuring-bind (&optional (length '*)) rest
                         (array-ctype 't (list length) env)))
      ((single-float) (destructuring-bind (&optional (low '*) (high '*)) rest
                        (range-ctype 'single-float low high env)))
      #+(or)
      ((string) ...)
      ((unsigned-byte) (destructuring-bind (&optional (s '*)) rest
                         (range-ctype 'integer 0
                                      (if (eq s '*)
                                          '*
                                          (1- (expt 2 s)))
                                      env)))
      ((values) (parse-values-ctype rest env))
      ((vector) (destructuring-bind (&optional (length '*)) rest
                  (array-ctype '* (list length) env))))))

(defun specifier-ctype (specifier &optional env)
  (let ((spec (typexpand specifier env)))
    (etypecase spec
      (cons (cons-specifier-ctype (car spec) (cdr spec) env))
      (symbol (or (symbol-specifier-ctype spec env)
                  (class-specifier-ctype (find-class specifier t env) env)))
      (class (or (symbol-specifier-ctype (class-name spec) env)
                 (class-specifier-ctype spec env))))))
