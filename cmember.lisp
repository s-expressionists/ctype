(in-package #:ctype)

(defmethod ctypep (client object (ct cmember))
  (declare (ignore client))
  (member object (cmember-members ct)))

(defmethod subctypep (client (ct1 cmember) (ct2 ctype))
  (values (loop for m in (cmember-members ct1)
                always (ctypep client m ct2))
          t))
(defmethod subctypep (client (ct1 cmember) (ct2 cmember))
  (declare (ignore client))
  (values (subsetp (cmember-members ct1) (cmember-members ct2)) t))

(define-commutative-method disjointp (client (cmember cmember) (ctype ctype))
  (values
   (notany (lambda (single-member)
             (ctypep client single-member ctype))
           (cmember-members cmember))
   t))

(defmethod conjointp (client (ct1 cmember) (ct2 cmember))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct cmember))
  (declare (ignore client))
  (values nil t))

(defmethod conjoin/2 (client (ct1 cmember) (ct2 cmember))
  (declare (ignore client))
  (apply #'cmember
         (intersection (cmember-members ct1) (cmember-members ct2))))

(define-commutative-method conjoin/2 (client (cmember cmember) (ctype ctype))
  ;; FIXME: Could save a little consing by checking subctypep first I guess.
  (apply #'cmember
         (loop for mem in (cmember-members cmember)
               when (ctypep client mem ctype) collect mem)))

(defmethod disjoin/2 (client (ct1 cmember) (ct2 cmember))
  (declare (ignore client))
  (apply #'cmember (union (cmember-members ct1) (cmember-members ct2))))

(define-commutative-method disjoin/2 (client (cmember cmember) (ctype ctype))
  (declare (ignore client))
  (let ((non (loop with diff = nil
                   for mem in (cmember-members cmember)
                   if (ctypep mem ctype)
                     do (setf diff t)
                   else
                     collect mem
                   ;; If there's no change, give up to avoid recursion
                   finally (unless diff (return-from disjoin/2 nil)))))
    (if non
        (disjunction (apply #'cmember non) ctype)
        ctype)))

(defmethod subtract (client (ct1 cmember) (ct2 cmember))
  (declare (ignore client))
  (apply #'cmember
         (set-difference (cmember-members ct1) (cmember-members ct2))))
(defmethod subtract (client (ct1 cmember) (ct2 ctype))
  (apply #'cmember
         (loop with some = nil
               for mem in (cmember-members ct1)
               if (ctypep client mem ct2)
                 do (setf some t)
               else
                 collect mem
               finally (unless some (return-from subtract ct1)))))
(defmethod subtract (client (ct1 ctype) (ct2 cmember))
  (let ((new
          (loop with never = t ; are all members not of the ctype?
                with diff = nil ; is some member not of the ctype?
                for mem in (cmember-members ct2)
                if (ctypep client mem ct1)
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
