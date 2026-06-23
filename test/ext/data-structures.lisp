(in-package #:ctype.test)

(5am:def-suite data-structures :in ext)
(5am:in-suite data-structures)

;;; todo? try parsing?
(defun carray-of (&rest args)
  (apply #'ctype.ext.data-structures:carray-of
         ctype-extrinsic:*client* args))

(5am:test carray-subtype
  (macrolet ((sub (sub surety et1 et2)
               `(5am:is (equal '(,sub ,surety)
                               (multiple-value-list
                                (ctype:subctypep ctype-extrinsic:*client*
                                                 (carray-of
                                                  (ctype-extrinsic:specifier-ctype ',et1))
                                                 (carray-of
                                                  (ctype-extrinsic:specifier-ctype ',et2))))))))
    ;; test things that would not be subtypes with standard array upgrading
    (sub t t bit t)
    (sub nil t t bit)
    (sub t t base-char t)
    (sub nil t t base-char)
    (sub t t base-char character)
    (sub t t character t)
    (sub nil t t character)
    ;; things that could technically not be upgraded but probably would be
    (sub t t (single-float 0.0 1.0) single-float)
    (sub nil t single-float (single-float 0.0 1.0))
    (sub t t (unsigned-byte 4) (signed-byte 57))
    (sub nil t (signed-byte 57) (unsigned-byte 4))))

(5am:test carray-disjoint
  (macrolet ((dis (sub surety et1 et2)
               `(5am:is (equal '(,sub ,surety)
                               (multiple-value-list
                                (ctype:disjointp ctype-extrinsic:*client*
                                                 (carray-of
                                                  (ctype-extrinsic:specifier-ctype ',et1))
                                                 (carray-of
                                                  (ctype-extrinsic:specifier-ctype ',et2))))))))
    (dis t t bit base-char)
    (dis t t single-float (unsigned-byte 8))
    (dis nil t bit t)
    (dis nil t base-char character)
    (dis nil t character t)))

;;; make sure carray still respects uaet if it's provided
(5am:test carray-uaet
  (macrolet ((sub (sub surety et1 uaet1 et2 uaet2)
               `(5am:is (equal '(,sub ,surety)
                               (multiple-value-list
                                (ctype:subctypep ctype-extrinsic:*client*
                                                 (carray-of
                                                  (ctype-extrinsic:specifier-ctype ',et1)
                                                  '* ',uaet1)
                                                 (carray-of
                                                  (ctype-extrinsic:specifier-ctype ',et2)
                                                  '* ',uaet2)))))))
    (sub nil t bit bit t t)
    (sub nil t character character t t)
    (sub t t (unsigned-byte 4) (signed-byte 16) (signed-byte 7) (signed-byte 16))
    (sub nil t
         (unsigned-byte 8) (signed-byte 16) (signed-byte 7) (signed-byte 16))))
