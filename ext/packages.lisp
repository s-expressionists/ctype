(defpackage #:ctype.ext.data-structures
  (:use #:cl #:ctype)
  (:import-from #:alexandria
                #:map-product
                #:iota)
  ;; ctypes
  (:export
   #:clist-of
   #:element-ctype
   #:carray-of
   #:carray-simplicity
   #:carray-uaet
   #:carray-eaet
   #:carray-dims
   #:chash-table-of
   #:key-ctype
   #:value-ctype
   #:cproperty-list
   #:key-ctypes
   #:keys
   #:property-ctype)
  ;; extended types
  (:export
   #:list-of
   #:array-of
   #:simple-array-of
   #:vector-of
   #:simple-vector-of
   #:hash-table-of
   #:plist))
