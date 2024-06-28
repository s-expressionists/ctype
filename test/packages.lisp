(defpackage #:ctype.test
  (:use #:cl)
  (:shadow #:most-positive-fixnum #:most-negative-fixnum
           #:upgraded-array-element-type)
  (:export #:run!))
