(defpackage #:ctype.test
  (:use #:cl #:ctype-extrinsic)
  (:shadowing-import-from #:ctype-extrinsic
                          #:typep #:subtypep
                          #:upgraded-array-element-type
                          #:upgraded-complex-part-type))
