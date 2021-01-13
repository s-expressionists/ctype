(in-package #:ctype)

(defmethod ctypep (object (ct cmember))
  (member object (cmember-members ct)))

(defmethod subctypep ((ct1 cmember) (ct2 ctype))
  (values (loop for m in (cmember-members ct1)
                always (ctypep m ct2))
          t))
(defmethod subctypep ((ct1 cmember) (ct2 cmember))
  (values (subsetp (cmember-members ct1) (cmember-members ct2)) t))

(defmethod conjoin/2 ((ct1 cmember) (ct2 cmember))
  (let ((new (intersection (cmember-members ct1) (cmember-members ct2))))
    (if new
        (apply #'cmember new)
        (bot))))
(defun conjoin-cmember (cmember ctype)
  ;; FIXME: Could save a little consing by checking subctypep first I guess.
  (let ((new (loop for mem in (cmember-members cmember)
                   when (ctypep mem ctype)
                     collect mem)))
    (if new
        (apply #'cmember new)
        (bot))))
(defmethod conjoin/2 ((ct1 cmember) (ct2 ctype)) (conjoin-cmember ct1 ct2))
(defmethod conjoin/2 ((ct1 ctype) (ct2 cmember)) (conjoin-cmember ct2 ct1))

(defmethod disjoin/2 ((ct1 cmember) (ct2 cmember))
  (apply #'cmember (union (cmember-members ct1) (cmember-members ct2))))

(defmethod subtract ((ct1 cmember) (ct2 cmember))
  (let ((new (set-difference (cmember-members ct1) (cmember-members ct2))))
    (if new
        (apply #'cmember new)
        (bot))))
(defmethod subtract ((ct1 cmember) (ct2 ctype))
  (let ((new
          (loop with some = nil
                for mem in (cmember-members ct1)
                if (ctypep mem ct2)
                  do (setf some t)
                else
                  collect mem
                finally (unless some (return-from subtract nil)))))
    (if new
        (apply #'cmember new)
        (bot))))
(defmethod subtract ((ct1 ctype) (ct2 cmember))
  (let ((new
          (loop with never = t ; are all members not of the ctype?
                with diff = nil ; is some member not of the ctype?
                for mem in (cmember-members ct2)
                if (ctypep mem ct1)
                  collect mem
                  and do (setf never nil)
                else do (setf diff t)
                finally (cond (never (return-from subtract ct1))
                              ((not diff) (return-from subtract nil))))))
    (conjunction ct1 (negation (apply #'cmember new)))))

(defmethod unparse ((ct cmember))
  (let ((mems (cmember-members ct)))
    (cond ((equal mems '(nil)) 'null)
          ((null (rest mems)) `(eql ,(first mems)))
          (t `(member ,@(cmember-members ct))))))
