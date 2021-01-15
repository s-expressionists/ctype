(in-package #:ctype)

(defmethod ctypep (object (ct cvalues))
  (declare (ignore object))
  (error "Values ctype ~a cannot be used with ~a" ct 'ctypep))

(defmethod subctypep ((ct1 cvalues) (ct2 cvalues))
  (let ((req1 (cvalues-required ct1)) (req2 (cvalues-required ct2))
        (opt1 (cvalues-optional ct1)) (opt2 (cvalues-optional ct2))
        (rest1 (cvalues-rest ct1)) (rest2 (cvalues-rest ct2)))
    (if (> (length req2) (length req1))
        (values nil t)
        (loop with surety = t
              for sct1 = (or (pop req1) (pop opt1) rest1)
              for sct2 = (or (pop req2) (pop opt2) rest2)
              do (multiple-value-bind (val subsurety)
                     (subctypep sct1 sct2)
                   (cond ((not subsurety) (setf surety nil))
                         ((not val) (return-from subctypep (values nil t)))))
              until (and (null req1) (null req2) (null opt1) (null opt2))
              finally (return (if surety (values t t) (call-next-method)))))))

(defmethod conjoin/2 ((ct1 cvalues) (ct2 cvalues))
  (let ((req1 (cvalues-required ct1)) (req2 (cvalues-required ct2))
        (opt1 (cvalues-optional ct1)) (opt2 (cvalues-optional ct2))
        (rest1 (cvalues-rest ct1)) (rest2 (cvalues-rest ct2)))
    (let* ((req (loop for sct1 = (or (pop req1) (pop opt1) rest1)
                      for sct2 = (or (pop req2) (pop opt2) rest2)
                      for conj = (conjoin sct1 sct2)
                      if (bot-p conj)
                        do (return-from conjoin/2 conj)
                      else collect conj
                      until (and (null req1) (null req2))))
           (opt (loop for sct1 = (or (pop opt1) rest1)
                      for sct2 = (or (pop opt2) rest2)
                      for conj = (conjoin sct1 sct2)
                      if (bot-p conj)
                        ;; This &optional is bottom, and so neither this value
                        ;; nor any later values can be provided.
                        do (return-from conjoin/2 (cvalues req opts conj))
                      else collect conj into opts
                      until (and (null opt1) (null opt2))
                      finally (return opts)))
           (rest (conjoin rest1 rest2)))
      (cvalues req opt rest))))

;;; Disjunctions are much more limited; for example
;;; (or (values null null) (values cons cons))
;;; is a strict subtype of (values list list), which additionally includes
;;; (values nil '(nil)) and such.
;;; If we canonicalize single-value types into not-cvalues I think we don't
;;; really need to handle values disjunctions specially.
;;; Negation and subtraction are complicated for similar reasons.

(defmethod unparse ((ct cvalues))
  `(values ,@(mapcar #'unparse (cvalues-required ct))
           ,@(let ((opt (cvalues-optional ct)))
               (when opt
                 `(&optional ,@(mapcar #'unparse opt))))
           &rest ,(unparse (cvalues-rest ct))))
