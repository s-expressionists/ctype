(in-package #:ctype)

;;;; a lot of the code in this file is cribbed from sbcl's character-set types.

(defmethod ctypep (client object (ct charset))
  (declare (ignore client))
  (and (characterp object)
       (loop with code = (char-code object)
             for (begin . end) in (charset-pairs ct)
             when (<= begin code end)
               return t
             finally (return nil))))

(defmethod subctypep (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  (values
   (flet ((subrangep (pair1 pair2)
            (let ((low1 (car pair1)) (high1 (cdr pair1))
                  (low2 (car pair2)) (high2 (cdr pair2)))
              (and (>= low1 low2) (<= high1 high2)))))
     (loop with pairs2 = (charset-pairs ct2)
           for pair1 in (charset-pairs ct1)
           always (position pair1 pairs2 :test #'subrangep)))
   t))

(defmethod ctype= (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  (values (equal (charset-pairs ct1) (charset-pairs ct2)) t))

(defmethod disjointp (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  (values
   (flet ((overlap-p (pair1 pair2)
            (let ((low1 (car pair1)) (high1 (cdr pair1))
                  (low2 (car pair2)) (high2 (cdr pair2)))
              (and (<= low1 high2) (<= low2 high1)))))
     (loop with pairs2 = (charset-pairs ct2)
           for pair1 in (charset-pairs ct1)
           never (position pair1 pairs2 :test #'overlap-p)))
   t))
(defmethod conjointp (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  (values nil t))

(defmethod cofinitep (client (ct charset))
  (declare (ignore client))
  (values nil t))

(defun negate-charset-pairs (pairs)
  (if (null pairs)
      `((0 . ,(1- char-code-limit)))
      (let ((not-pairs nil))
        (when (plusp (caar pairs))
          (push (cons 0 (1- (caar pairs))) not-pairs))
        (loop for tail on pairs
              for high1 = (cdar tail)
              for low2 = (caadr tail)
              until (null (rest tail))
              do (push (cons (1+ high1) (1- low2)) not-pairs)
              finally (when (< (cdar tail) (1- char-code-limit))
                        (push (cons (1+ (cdar tail)) (1- char-code-limit))
                              not-pairs))
                      (return (nreverse not-pairs))))))

(defmethod negate (client (ct charset))
  (declare (ignore client))
  (let ((pairs (charset-pairs ct)))
    (if (equal pairs `((0 . ,(1- char-code-limit))))
        (call-next-method)
        (let ((not-character
                (negation
                 (charset `((0 . ,(1- char-code-limit)))))))
          (disjunction
           not-character
           (charset (negate-charset-pairs pairs)))))))

(defun conjoin-charset-pairs (pairs1 pairs2)
  (if (and pairs1 pairs2)
      (let ((res nil)
            (pair1 (pop pairs1))
            (pair2 (pop pairs2)))
        (loop
          ;; Put the higher pair on the right.
          (when (> (car pair1) (car pair2))
            (rotatef pair1 pair2)
            (rotatef pairs1 pairs2))
          (let ((pair1-high (cdr pair1)))
            (cond
              ((> (car pair2) pair1-high)
               ;; no overlap -- discard pair1 and move on
               (if (null pairs1)
                   (return)
                   (setf pair1 (pop pairs1))))
              ((<= (cdr pair2) pair1-high)
               ;; pair2 is a subrange of pair1
               (push (cons (car pair2) (cdr pair2)) res)
               (cond
                 ((= (cdr pair2) pair1-high)
                  ;; both pairs are now in the result, so advance both
                  (if (null pairs1)
                      (return)
                      (setf pair1 (pop pairs1)))
                  (if (null pairs2)
                      (return)
                      (setf pair2 (pop pairs2))))
                 (t
                  ;; (< (cdr pair2) pair1-high)
                  ;; so pair2 is a strict subrange of pair1 - advance 2 only,
                  ;; and "modify" pair1 accordingly
                  (if (null pairs2)
                      (return)
                      (setf pair2 (pop pairs2)))
                  (setf pair1 (cons (1+ (cdr pair2)) pair1-high)))))
              (t
               ;; (> (cdr pair2) (cdr pair1))
               ;; so the ranges overlap, but only partially.
               ;; push the overlap and advance 2 and modify 1.
               (push (cons (car pair2) pair1-high) res)
               (if (null pairs1)
                   (return)
                   (setf pair1 (pop pairs1)))
               (setf pair2 (cons (1+ pair1-high) (cdr pair2)))))))
        ;; done
        (nreverse res))
      ;; One of the charsets is degenerate (empty)
      ;; which ought to have been normalized away, but as long as we're here
      nil))

(defmethod conjoin/2 (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  (charset (conjoin-charset-pairs (charset-pairs ct1) (charset-pairs ct2))))

(defun disjoin-charset-pairs (pairs1 pairs2)
  (cond
    ((not pairs1) pairs2)
    ((not pairs2) pairs1)
    (t
     (let ((res nil))
       (loop (let* ((current (if (> (caar pairs2) (caar pairs1))
                                 (pop pairs1)
                                 (pop pairs2)))
                    (low (car current)) (high (cdr current)))
               ;; Keep grabbing overlapping pairs until we run out.
               (loop (cond ((and pairs1
                                 (<= (caar pairs1) (1+ high)))
                            (setf high (max high (cdr (pop pairs1)))))
                           ((and pairs2
                                 (<= (caar pairs2) (1+ high)))
                            (setf high (max high (cdr (pop pairs2)))))
                           (t (return)))) ; ran out
               (push (cons low high) res)
               ;; Check to see if we're really done.
               (unless (or pairs1 pairs2)
                 (return (nreverse res)))))))))

(defmethod disjoin/2 (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  (charset (disjoin-charset-pairs (charset-pairs ct1) (charset-pairs ct2))))

(defmethod subtract (client (ct1 charset) (ct2 charset))
  (declare (ignore client))
  ;; lazy
  (charset (conjoin-charset-pairs (charset-pairs ct1)
                                  (negate-charset-pairs (charset-pairs ct2)))))

(defmethod unparse ((ct charset))
  (let ((pairs (charset-pairs ct)))
    (cond ((equal pairs +standard-charset+)
           'standard-char)
          ((equal pairs +base-charset+)
           'base-char)
          ((equal pairs `((0 . ,(1- char-code-limit))))
           'character)
          ((equal pairs (negate-charset-pairs +standard-charset+))
           '(and character (not standard-char)))
          ((equal pairs (negate-charset-pairs +base-charset+))
           'extended-char)
          (t ; something weird. do a member type.
           `(member
             ,@(loop for (low . high) in pairs
                     nconc (loop for i from low upto high
                                 collect (code-char i))))))))
