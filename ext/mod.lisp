(defpackage #:ctype.ext.mod
  (:use #:cl #:ctype)
  (:export #:congruence))

(in-package #:ctype.ext.mod)

(defclass congruence (ctype)
  ((%modulus :initarg :modulus :reader modulus :type (integer 2))
   ;; A field of MODULUS bits. The Nth bit being set indicates that this type
   ;; includes integers that are N mod MODULUS.
   (%congruences :initarg :congruences :reader congruences
                 :type (integer 1))))

(defun %congruence (modulus congruences)
  (make-instance 'congruence :modulus modulus :congruences congruences))

(defun canonicalize-congruence (modulus congruences)
  ;; This function is to avoid "degenerate" congruences. A congruence is
  ;; degenerate if it is equivalent to a congruence with a smaller modulus.
  ;; For example, {1, 3} mod 4 is the same as {1} mod 2, so it's degenerate.
  ;; In more detail, if our bitfield is repetitive,
  ;; the congruence is degenerate.
  ;; Avoiding degeneracies makes some computations easier.
  (loop for i from 2 upto (floor modulus 2)
        when (multiple-value-bind (div mod) (floor modulus i)
               (and (zerop mod)
                    (loop with byte = (byte i 0)
                          with canon = (ldb byte congruences)
                          for subcong = congruences
                            then (ash subcong (- i))
                          repeat div
                          always (= (ldb byte subcong) canon))))
          return (values i (ldb (byte i 0) congruences))
        finally (return (values modulus congruences))))

(defun congruence (modulus congruences)
  (cond ((zerop congruences) (bot)) ; empty
        ((= congruences (1- (ash 1 modulus))) ; full
         (range 'integer nil nil nil nil))
        (t (multiple-value-bind (modulus congruences)
               (canonicalize-congruence modulus congruences)
             (%congruence modulus congruences)))))

(defmethod ctypep (object (ct congruence))
  (and (integerp object)
       (logbitp (mod object (modulus ct)) (congruences ct))))

(defmethod subctypep ((ct1 congruence) (ct2 congruence))
  (let ((mod1 (modulus ct1)) (cong1 (congruences ct1))
        (mod2 (modulus ct2)) (cong2 (congruences ct2)))
    ;; We assume there are no degenerate congruences.
    ;; As such, there is no possibility of a mod4 being a subtype of a mod2,
    ;; for example.
    (multiple-value-bind (div mod) (truncate mod2 mod1)
      (and (zerop mod)
           ;; Imagine we have a mod2 and a mod4.
           ;; The mod2 is a subtype of the mod4 only if the mod4 has
           ;; all the congruences of the mod2.
           ;; E.g., if it's {1} mod2, we'd need at least {1,3} mod4.
           ;; To compute this, we take sections of cong2 that are mod1 long,
           ;; and ensure that each has at least the bits set that cong1 does.
           (dotimes (i div (values t t))
             (unless (zerop (logandc2 cong1 cong2))
               (return-from subctypep (values nil t)))
             (setf cong1 (ash cong2 (- mod1))))))))

(defmethod ctype= ((ct1 congruence) (ct2 congruence))
  ;; Again, we need the canonicalization above for this to be valid
  (and (= (modulus ct1) (modulus ct2))
       (= (congruences ct1) (congruences ct2))))

(defmethod disjointp ((ct1 congruence) (ct2 congruence))
  (let ((mod1 (modulus ct1)) (cong1 (congruences ct1))
        (mod2 (modulus ct2)) (cong2 (congruences ct2)))
    (let ((mod (lcm mod1 mod2)))
      ;; NOTE: The truncations are even
      (values
       (zerop (logand (repeat-bits cong1 mod1 (truncate mod mod1))
                      (repeat-bits cong2 mod2 (truncate mod mod2))))
       t))))

(defmethod conjointp ((ct1 congruence) (ct2 congruence)) (values nil t))
(defmethod cofinitep ((ct1 congruence)) (values nil t))

(defmethod negate ((ct congruence))
  ;; (not (satisfies evenp)) = (or (not integer) (satisfies oddp))
  (disjunction (negation (range 'integer nil nil nil nil))
               (let ((m (modulus ct)))
                 ;; if the input is nondegenerate the negation must be too,
                 ;; so skip canonicalization
                 (%congruence m (ldb (byte m 0) (lognot (congruences ct)))))))

;; (repeat-bits #b101 3 4) => #b101101101101
(defun repeat-bits (bits len repeats)
  (let ((byte (ldb (byte len 0) bits)))
    (loop repeat repeats
          for result = byte
            then (logior (ash result len) byte)
          finally (return result))))

(defmethod conjoin/2 ((ct1 congruence) (ct2 congruence))
  (let ((mod1 (modulus ct1)) (cong1 (congruences ct1))
        (mod2 (modulus ct2)) (cong2 (congruences ct2)))
    (let* ((mod (lcm mod1 mod2))
           ;; NOTE: The truncations are even
           (cong (logand (repeat-bits cong1 mod1 (truncate mod mod1))
                         (repeat-bits cong2 mod2 (truncate mod mod2)))))
      (congruence mod cong))))

(defmethod disjoin/2 ((ct1 congruence) (ct2 congruence))
  (let ((mod1 (modulus ct1)) (cong1 (congruences ct1))
        (mod2 (modulus ct2)) (cong2 (congruences ct2)))
    (let* ((mod (lcm mod1 mod2))
           ;; NOTE: The truncations are even
           (cong (logior (repeat-bits cong1 mod1 (truncate mod mod1))
                         (repeat-bits cong2 mod2 (truncate mod mod2)))))
      (congruence mod cong))))

(defmethod subtract ((ct1 congruence) (ct2 congruence))
  (let ((mod1 (modulus ct1)) (cong1 (congruences ct1))
        (mod2 (modulus ct2)) (cong2 (congruences ct2)))
    (let* ((mod (lcm mod1 mod2))
           ;; NOTE: The truncations are even
           (cong (logandc2 (repeat-bits cong1 mod1 (truncate mod mod1))
                           (repeat-bits cong2 mod2 (truncate mod mod2)))))
      (congruence mod cong))))

(defmethod print-object ((object congruence) stream)
  (print-unreadable-object (object stream :type t)
    (let ((mod (modulus object)) (cong (congruences object)))
      (write (loop for i below mod
                   when (logbitp i cong)
                     collect i)
             :stream stream)
      (write-char #\Space stream)
      (write 'cl:mod :stream stream)
      (write-char #\Space stream)
      (write mod :stream stream)))
  object)

;;;

(defmethod subctypep ((ct1 congruence) (ct2 range))
  (values (and (eq (range-kind ct2) 'integer)
               (null (range-low ct2))
               (null (range-high ct2)))
          t))
(defmethod subctypep ((ct1 range) (ct2 congruence))
  ;; FIXME: This is pretty inaccurate; e.g. we could check small ranges.
  (if (or (not (eq (range-kind ct1) 'integer))
          ;; A range unbounded on one side necessarily includes all
          ;; congruences at some point, and we don't allow all-inclusive
          ;; congruence types like that.
          (null (range-low ct1))
          (null (range-high ct1)))
      (values nil t)
      (values nil nil)))

(define-commutative-method disjointp (congruence congruence) (range range)
  ;; FIXME: Inaccurate in basically the same way.
  (if (or (not (eq (range-kind range) 'integer))
          ;; A range unbounded on one side necessarily includes all
          ;; congruences at some point, and we don't allow all-inclusive
          ;; congruence types like that.
          (null (range-low range))
          (null (range-high range)))
      (values t t)
      (values nil nil)))

(define-commutative-method conjointp (ct1 congruence) (ct2 range)
  (values nil t))

(defexclusives congruence cclass ccomplex carray charset cfunction fpzero)
