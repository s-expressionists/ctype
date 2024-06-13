(in-package #:ctype)

(defun lambda-list-top-p (lambda-list)
  (and (null (lambda-list-required lambda-list))
       (every #'top-p (lambda-list-optional lambda-list))
       (top-p (lambda-list-rest lambda-list))
       (not (lambda-list-keyp lambda-list))))

;;; Does this function ctype = FUNCTION unadorned?
(defun function-top-p (cfunction)
  (and (lambda-list-top-p (cfunction-lambda-list cfunction))
       (values-top-p (cfunction-returns cfunction))))

(defmethod ctypep (client object (ct cfunction))
  (declare (ignore client))
  (if (functionp object)
      (if (function-top-p ct)
          t
          (error "Cannot use complex function type ~a for ~a"
                 ct 'ctypep))
      nil))

(defun sub-lambda-list-p (client ll1 ll2)
  (let ((req1 (lambda-list-required ll1)) (req2 (lambda-list-required ll2))
        (rest1 (lambda-list-rest ll1)) (rest2 (lambda-list-rest ll2))
        (surety t))
    (when (> (length req2) (length req1))
      (return-from sub-lambda-list-p (values nil t)))
    (multiple-value-bind (val subsurety) (subctypep client rest1 rest2)
      (unless val
        (if subsurety
            (return-from sub-lambda-list-p (values nil t))
            (setf surety nil))))
    (loop with opt1 = (lambda-list-optional ll1)
          with opt2 = (lambda-list-optional ll2)
          for sct1 = (or (pop req1) (pop opt1) rest1)
          for sct2 = (or (pop req2) (pop opt2) rest2)
          do (multiple-value-bind (val subsurety) (subctypep client sct1 sct2)
               (cond ((not subsurety) (setf surety nil))
                     ((not val) (return-from sub-lambda-list-p
                                  (values nil t)))))
          until (and (null req1) (null opt1) (null req2) (null opt2)))
    ;: TODO
    (when (or (lambda-list-keyp ll1) (lambda-list-keyp ll2))
      (setf surety nil))
    (if surety
        (values t t)
        (values nil nil))))

(defmethod subctypep (client (ct1 cfunction) (ct2 cfunction))
  (multiple-value-bind (val1 surety1)
      (subctypep client (cfunction-returns ct1) (cfunction-returns ct2))
    (if (and surety1 (not val1))
        (values nil t)
        (multiple-value-bind (val2 surety2)
            (sub-lambda-list-p client
                               (cfunction-lambda-list ct1)
                               (cfunction-lambda-list ct2))
          (cond ((not surety2) (values nil nil))
                ((not val2) (values nil t))
                (surety1 (values t t))
                (t (values nil nil)))))))

(defmethod cofinitep (client (ct cfunction))
  (declare (ignore client))
  (values nil t))

(defun lambda-list-conjoin (client ll1 ll2)
  (let* ((req1 (lambda-list-required ll1)) (req2 (lambda-list-required ll2))
         (opt1 (lambda-list-optional ll1)) (opt2 (lambda-list-optional ll2))
         (rest1 (lambda-list-rest ll1)) (rest2 (lambda-list-rest ll2))
         (req (if (or req1 req2)
                  (loop for sct1 = (or (pop req1) (pop opt1) rest1)
                        for sct2 = (or (pop req2) (pop opt2) rest2)
                        for conj = (conjoin client sct1 sct2)
                        if (bot-p conj)
                          do (return-from lambda-list-conjoin conj)
                        else collect conj
                        until (and (null req1) (null req2)))
                  nil))
         (rest (conjoin client rest1 rest2))
         (opt (if (or opt1 opt2)
                  (loop for sct1 = (or (pop opt1) rest1)
                        for sct2 = (or (pop opt2) rest2)
                        for conj = (conjoin client sct1 sct2)
                        if (bot-p conj)
                          do (setf rest conj)
                             (loop-finish)
                        else collect conj
                        until (and (null opt1) (null opt2)))
                  nil)))
    (if (or (lambda-list-keyp ll1) (lambda-list-keyp ll2))
        ;; TODO
        nil
        (make-instance 'lambda-list :required req :optional opt :rest rest
                       :keyp nil :keys nil :aokp nil))))

(defmethod conjoin/2 (client (ct1 cfunction) (ct2 cfunction))
  (let ((ll (lambda-list-conjoin client
                                 (cfunction-lambda-list ct1)
                                 (cfunction-lambda-list ct2)))
        ;; We use conjoin/2 rather than conjoin because we know both are values
        ;; types, and that conjoin/2 always simplifies such types.
        (rv (conjoin/2 client
                       (cfunction-returns ct1)
                       (cfunction-returns ct2))))
    (cond ((bot-p ll) ll)
          ((and ll rv)
           (make-instance 'cfunction :lambda-list ll :returns rv))
          (t nil))))

(defun unparse-lambda-list (lambda-list)
  (if (lambda-list-top-p lambda-list)
      '*
      `(,@(mapcar #'unparse (lambda-list-required lambda-list))
        ,@(let ((opt (lambda-list-optional lambda-list)))
            (when opt
              `(&optional ,@(mapcar #'unparse opt))))
        ,@(let ((rest (lambda-list-rest lambda-list)))
            (unless (bot-p rest)
              `(&rest ,(unparse rest))))
        ,@(when (lambda-list-keyp lambda-list) '(&key))
        ,@(loop for (keyword . ctype) in (lambda-list-key lambda-list)
                collect `(,keyword ,(unparse ctype)))
        ,@(when (lambda-list-aokp lambda-list) '(&allow-other-keys)))))

(defmethod unparse ((ct cfunction))
  (let ((ull (unparse-lambda-list (cfunction-lambda-list ct)))
        (rv (cfunction-returns ct)))
    (if (values-top-p rv)
        (if (eq ull '*)
            'function
            `(function ,ull))
        `(function ,ull ,(unparse rv)))))
