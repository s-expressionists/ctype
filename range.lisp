(in-package #:ctype)

(defmethod ctypep (object (ct range))
  (and (number-kindp object (range-kind ct))
       (let ((low (range-low ct)))
         (or (not low)
             (if (range-low-exclusive-p ct)
                 (< low object)
                 (<= low object))))
       (let ((high (range-high ct)))
         (or (not high)
             (if (range-high-exclusive-p ct)
                 (< object high)
                 (<= object high))))))

(defmethod subctypep ((ct1 range) (ct2 range))
  (values
   (and (eq (range-kind ct1) (range-kind ct2))
        (let ((low1 (range-low ct1)) (low2 (range-low ct2)))
          (or (not low2)
              (and low1
                   (or (< low2 low1)
                       (and (= low2 low1)
                            (or (range-low-exclusive-p ct1)
                                (not (range-low-exclusive-p ct2))))))))
        (let ((high1 (range-high ct1)) (high2 (range-high ct2)))
          (or (not high2)
              (and high1
                   (or (< high1 high2)
                       (and (= high1 high2)
                            (or (range-low-exclusive-p ct1)
                                (not (range-high-exclusive-p ct2)))))))))
   t))

(defmethod negate ((ct range))
  ;; (not (real x (y))) = (or (not real) (real * (x)) (real y *))
  (let* ((kind (range-kind ct))
         (negk (negation (make-instance 'range
                           :kind kind :low nil :lxp nil :high nil :hxp nil)))
         (low (range-low ct)) (high (range-high ct))
         (lxp (range-low-exclusive-p ct)) (hxp (range-high-exclusive-p ct)))
    (cond ((and low high)
           (disjunction
            negk
            (make-instance 'range
              :kind kind :low nil :lxp nil :high low :hxp (not lxp))
            (make-instance 'range
              :kind kind :low high :lxp (not hxp) :high nil :hxp nil)))
          (low
           (disjunction
            negk
            (make-instance 'range
              :kind kind :low nil :lxp nil :high low :hxp (not lxp))))
          (high
           (disjunction
            negk
            (make-instance 'range
              :kind kind :low high :lxp (not hxp) :high nil :hxp nil)))
          (t negk))))

(defmethod conjoin/2 ((ct1 range) (ct2 range))
  (if (eq (range-kind ct1) (range-kind ct2))
      (multiple-value-bind (low lxp)
          (let ((low1 (range-low ct1)) (low2 (range-low ct2))
                (lxp1 (range-low-exclusive-p ct1))
                (lxp2 (range-low-exclusive-p ct2)))
            (cond ((not low1) (values low2 lxp2))
                  ((not low2) (values low1 lxp1))
                  ((< low1 low2) (values low2 lxp2))
                  ((< low2 low1) (values low1 lxp1))
                  (t (values low1 (or lxp1 lxp2)))))
        (multiple-value-bind (high hxp)
            (let ((high1 (range-high ct1)) (high2 (range-high ct2))
                  (hxp1 (range-high-exclusive-p ct1))
                  (hxp2 (range-high-exclusive-p ct2)))
              (cond ((not high1) (values high2 hxp2))
                    ((not high2) (values high1 hxp1))
                    ((< high1 high2) (values high1 hxp1))
                    ((< high2 high1) (values high2 hxp2))
                    (t (values high1 (or hxp1 hxp2)))))
          (if (and low high
                   (or (> low high)
                       (and (= low high) (or lxp hxp))))
              ;; bounds have resulted in an empty range
              (bot)
              (make-instance 'range
                :kind (range-kind ct1) :low low :lxp lxp :high high :hxp hxp))))
      ;; Different kinds of range - conjunction is empty
      (bot)))

(defmethod disjoin/2 ((ct1 range) (ct2 range))
  (let ((rk1 (range-kind ct1)) (rk2 (range-kind ct2))
        (low1 (range-low ct1)) (low2 (range-low ct2))
        (lxp1 (range-low-exclusive-p ct1))
        (lxp2 (range-low-exclusive-p ct2))
        (high1 (range-high ct1)) (high2 (range-high ct2))
        (hxp1 (range-high-exclusive-p ct1))
        (hxp2 (range-high-exclusive-p ct2)))
    (if (and (eq rk1 rk2)
             (or (not high1) (not low2)
                 (> high1 low2)
                 (and (= high1 low2)
                      (or (not hxp1) (not lxp2)))))
        (multiple-value-bind (low lxp)
            (cond ((not low1) (values low1 lxp1))
                  ((not low2) (values low2 lxp2))
                  ((< low1 low2) (values low1 lxp1))
                  ((< low2 low1) (values low2 lxp2))
                  (t (values low1 (and lxp1 lxp2))))
          (multiple-value-bind (high hxp)
              (cond ((not high1) (values high1 hxp1))
                    ((not high2) (values high2 hxp2))
                    ((< high1 high2) (values high2 hxp2))
                    ((< high2 high1) (values high1 hxp1))
                    (t (values high1 (and hxp1 hxp2))))
            (make-instance 'range
              :kind rk1 :low low :lxp lxp :high high :hxp hxp)))
        ;; Different kinds of range or noncontiguous - punt
        (call-next-method))))

(defmethod unparse ((ct range))
  (let* ((kind (range-kind ct))
         (low (range-low ct)) (high (range-high ct))
         (ulow (cond ((not low) '*)
                     ((range-low-exclusive-p ct) (list low))
                     (t low)))
         (uhigh (cond ((not high) '*)
                      ((range-high-exclusive-p ct) (list high))
                      (t high)))
         (rest (if (eq uhigh '*)
                   (if (eq ulow '*)
                       nil
                       (list ulow))
                   (list ulow uhigh))))
    (if (eq kind 'ratio) ; no extended ratio type in CL, so we do stupid things
        `(and (not integer)
              ,@(if rest `((rational ,@rest)) '(rational)))
        (if rest `(,kind ,@rest) kind))))
