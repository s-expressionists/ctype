(defsystem :ctype
  :description "An implementation of the Common Lisp type system."
  :license "BSD"
  :author "Bike <aeshtaer@gmail.com>"
  :depends-on ()
  :components
  ((:file "packages")
   (:file "trivalent" :depends-on ("packages"))
   (:file "method-combination" :depends-on ("packages"))
   (:module "config"
    :serial t
    :components ((:file "common")
                 (:file "clasp" :if-feature :clasp)
                 (:file "sbcl"  :if-feature :sbcl)
                 (:file "ccl"   :if-feature :ccl)
                 (:file "cmucl" :if-feature :cmucl)
                 (:file "sicl"  :if-feature :sicl)
                 (:file "ecl"   :if-feature :ecl)
                 (:file "unsupported"
                  :if-feature (:not (:or :clasp :sbcl :ccl :cmucl :sicl :ecl)))
                 (:file "common-post")))
   (:file "classes" :depends-on ("packages"))
   (:file "create" :depends-on ("classes" "packages"))
   (:file "generic-functions"
    :depends-on ("trivalent" "method-combination" "create" "classes"
                             "packages"))
   (:file "cclass"
    :depends-on ("generic-functions" "classes" "config" "packages"))
   (:file "negation"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "conjunction"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "disjunction"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "ccons"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "range"
    :depends-on ("generic-functions" "create" "classes" "config" "packages"))
   (:file "fpzero"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "ccomplex"
    :depends-on ("generic-functions" "create" "classes" "config" "packages"))
   (:file "cmember"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "carray"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "charset"
    :depends-on ("generic-functions" "create" "classes" "config" "packages"))
   (:file "cvalues"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "cfunction"
    :depends-on ("cvalues" "generic-functions" "create" "classes" "packages"))
   (:file "csatisfies"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "pairwise"
    :depends-on ("generic-functions" "trivalent" "create" "classes"
                                     "cfunction" "packages"))
   (:file "parse"
    :depends-on ("generic-functions" "create" "classes" "config" "packages"))))

(asdf:defsystem :ctype/ext
  :license "BSD"
  :depends-on (:ctype :alexandria)
  :components
  ((:module "ext"
    :components
    ((:file "packages")
     (:module "data-structures"
      :depends-on ("packages")
      :components
      ((:file "list-of")
       (:file "array-of")
       (:file "hash-table-of")))))))
