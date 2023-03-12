(in-package #:ctype)

(defmethod ctypep (object (ct cmember))
  (member object (cmember-members ct)))

(defmethod subctypep ((ct1 cmember) (ct2 ctype))
  (values (loop for m in (cmember-members ct1)
                always (ctypep m ct2))
          t))
(defmethod subctypep ((ct1 cmember) (ct2 cmember))
  (values (subsetp (cmember-members ct1) (cmember-members ct2)) t))

(declaim (inline disjoint-member))
(defun disjoint-member (cmember ctype)
  (values
   (not (some (lambda (single-member)
                (ctypep single-member ctype))
              (cmember-members cmember)))
   t))

(defmethod disjointp ((cmember cmember) (ctype ctype))
  (disjoint-member cmember ctype))

(defmethod disjointp ((ctype ctype) (cmember cmember))
  (disjoint-member cmember ctype))

(defmethod conjointp ((ct1 cmember) (ct2 cmember)) (values nil t))

(defmethod cofinitep ((ct cmember)) (values nil t))

(defmethod conjoin/2 ((ct1 cmember) (ct2 cmember))
  (apply #'cmember
         (intersection (cmember-members ct1) (cmember-members ct2))))
(defun conjoin-cmember (cmember ctype)
  ;; FIXME: Could save a little consing by checking subctypep first I guess.
  (apply #'cmember
         (loop for mem in (cmember-members cmember)
               when (ctypep mem ctype) collect mem)))
(defmethod conjoin/2 ((ct1 cmember) (ct2 ctype)) (conjoin-cmember ct1 ct2))
(defmethod conjoin/2 ((ct1 ctype) (ct2 cmember)) (conjoin-cmember ct2 ct1))

(defmethod disjoin/2 ((ct1 cmember) (ct2 cmember))
  (apply #'cmember (union (cmember-members ct1) (cmember-members ct2))))
(defun disjoin-cmember (cmember ctype)
  (let ((non (loop with diff = nil
                   for mem in (cmember-members cmember)
                   if (ctypep mem ctype)
                     do (setf diff t)
                   else
                     collect mem
                   ;; If there's no change, give up to avoid recursion
                   finally (unless diff (return-from disjoin-cmember nil)))))
    (if non
        (disjunction (apply #'cmember non) ctype)
        ctype)))
(defmethod disjoin/2 ((ct1 cmember) (ct2 ctype))
  (disjoin-cmember ct1 ct2))
(defmethod disjoin/2 ((ct1 ctype) (ct2 cmember))
  (disjoin-cmember ct2 ct1))

(defmethod subtract ((ct1 cmember) (ct2 cmember))
  (apply #'cmember
         (set-difference (cmember-members ct1) (cmember-members ct2))))
(defmethod subtract ((ct1 cmember) (ct2 ctype))
  (apply #'cmember
         (loop with some = nil
               for mem in (cmember-members ct1)
               if (ctypep mem ct2)
                 do (setf some t)
               else
                 collect mem
               finally (unless some (return-from subtract ct1)))))
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
          ((or (equal mems '(nil t)) (equal mems '(t nil))) 'boolean)
          ((null (rest mems)) `(eql ,(first mems)))
          (t `(member ,@(cmember-members ct))))))
