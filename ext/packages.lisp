(defpackage #:ctype.ext.data-structures
  (:use #:cl #:ctype)
  (:import-from #:alexandria
                #:map-product
                #:iota)
  ;; ctypes
  (:export
   #:clist-of
   #:carray-of
   #:chash-table-of)
  ;; extended types
  (:export
   #:list-of
   #:array-of
   #:simple-array-of
   #:vector-of
   #:simple-vector-of
   #:hash-table-of))
