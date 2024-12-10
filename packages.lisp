(defpackage #:ctype
  (:use #:cl)
  (:export #:specifier-ctype #:values-specifier-ctype
           #:extended-specifier-ctype
           #:extended-values-specifier-ctype)
  (:export #:ctypep #:subctypep #:ctype=)
  (:export #:disjointp #:conjointp #:cofinitep)
  (:export #:negate #:conjoin/2 #:disjoin/2 #:subtract #:unparse
           #:conjoin #:disjoin)
  ;; Useful for extensions.
  (:export #:basic
           #:every/tri #:some/tri #:notevery/tri #:notany/tri
           #:and/tri #:or/tri #:surely
           #:defexistential #:defexclusives
           #:define-commutative-method
           #:define-extended-type
           #:+complex-arrays-exist-p+)
  ;; Interface to interrogate information about types.
  ;; EXPERIMENTAL, SUBJECT TO CHANGE.
  (:export #:ctype
           #:top #:bot #:top-p #:bot-p
           #:values-top #:values-bot #:values-top-p #:values-bot-p
           #:function-top #:lambda-list-top #:function-top-p
           #:cclass #:cclass-class
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
           #:sub-lambda-list-p
           #:cfunction #:cfunction-lambda-list #:cfunction-returns
           #:csatisfies #:csatisfies-fname))
