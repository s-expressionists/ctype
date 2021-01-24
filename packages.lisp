(defpackage #:ctype
  (:use #:cl)
  (:export #:specifier-ctype)
  (:export #:ctypep #:subctypep #:ctype=)
  (:export #:disjointp #:negate #:conjoin/2 #:disjoin/2 #:subtract #:unparse
           #:conjoin #:disjoin))
