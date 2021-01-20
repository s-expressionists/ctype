(defpackage #:ctype
  (:use #:cl)
  (:export #:specifier-ctype)
  (:export #:ctypep #:subctypep)
  (:export #:disjointp #:conjoin/2 #:disjoin/2 #:subtract #:unparse
           #:conjoin #:disjoin))
