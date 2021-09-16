(defpackage #:ctype
  (:use #:cl)
  (:export #:specifier-ctype)
  (:export #:ctypep #:subctypep #:ctype=)
  (:export #:disjointp #:conjointp #:cofinitep)
  (:export #:negate #:conjoin/2 #:disjoin/2 #:subtract #:unparse
           #:conjoin #:disjoin)
  ;; Interface to interrogate information about types.
  ;; EXPERIMENTAL, SUBJECT TO CHANGE.
  (:export #:cclass #:cclass-class
           #:negation #:negation-ctype
           #:conjunction #:disjunction #:junction-ctypes
           #:ccons #:ccons-car #:ccons-cdr
           #:range #:range-kind #:range-low #:range-high
           #:range-low-exclusive-p #:range-high-exclusive-p
           #:fpzero #:fpzero-kind #:fpzero-zero
           #:ccomplex #:ccomplex-ucpt
           #:cmember #:cmember-members
           #:carray #:carray-simplicity
           #:carray-uaet #:carray-eaet #:carray-dims
           #:charset #:charset-pairs
           #:cvalues #:cvalues-required #:cvalues-optional #:cvalues-rest
           #:lambda-list #:lambda-list-required #:lambda-list-optional
           #:lambda-list-rest #:lambda-list-keyp #:lambda-list-key
           #:lambda-list-aokp
           #:cfunction #:cfunction-lambda-list #:cfunction-returns
           #:csatisfies #:csatisfies-fname))
