(defsystem :ctype
  :description "An implementation of the Common Lisp type system."
  :license "BSD"
  :author "Bike <aeshtaer@gmail.com>"
  :depends-on ()
  :components
  ((:file "packages")
   (:file "trivalent" :depends-on ("packages"))
   (:file "method-combination" :depends-on ("packages"))
   (:file "client" :depends-on ("packages"))
   (:file "runtime" :depends-on ("client" "packages"))
   (:file "host" :depends-on ("client" "packages"))
   (:file "classes" :depends-on ("client" "packages"))
   (:file "create" :depends-on ("classes" "packages"))
   (:file "generic-functions"
    :depends-on ("trivalent" "method-combination" "create" "classes"
                             "packages"))
   (:file "cclass"
    :depends-on ("generic-functions" "classes" "client" "packages"))
   (:file "negation"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "conjunction"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "disjunction"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "ccons"
    :depends-on ("generic-functions" "create" "classes" "trivalent" "packages"))
   (:file "range"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "fpzero"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "ccomplex"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "cmember"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "carray"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "csequence"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "charset"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "cvalues"
    :depends-on ("generic-functions" "create" "classes" "packages"))
   (:file "cfunction"
    :depends-on ("cvalues" "generic-functions" "create" "classes" "packages"))
   (:file "csatisfies"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "pairwise"
    :depends-on ("generic-functions" "trivalent" "create" "classes"
                                     "cfunction" "client" "packages"))
   (:file "parse"
    :depends-on ("generic-functions" "create" "classes" "client" "packages"))
   (:file "interface"
    :depends-on ("generic-functions" "parse" "packages"))))

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
      ((:file "client")
       (:file "list-of" :depends-on ("client"))
       (:file "plist" :depends-on ("list-of" "client"))
       (:file "array-of" :depends-on ("client"))
       (:file "hash-table-of" :depends-on ("client"))))))))

(defsystem :ctype/tfun
  :description "Derived function return types for Common Lisp."
  :license "BSD"
  :author "Bike <aeshtaer@gmail.com>"
  :depends-on (:ctype :alexandria)
  :components
  ((:module "ext"
    :components
    ((:module "tfun"
      :components
      ((:file "packages")
       (:file "util" :depends-on ("packages"))
       (:file "tfun" :depends-on ("packages"))
       (:file "data-and-control-flow" :depends-on ("util" "tfun" "packages"))
       (:file "numbers" :depends-on ("util" "tfun" "packages"))
       (:file "conses" :depends-on ("util" "tfun" "packages"))
       (:file "arrays" :depends-on ("util" "tfun" "packages"))))))))

(defsystem #:ctype/test
  :author ("Bike <aeshtaer@gmail.com>")
  :depends-on (#:ctype #:fiveam)
  :components
  ((:module "test"
    :components ((:file "packages")
                 (:file "rt" :depends-on ("packages"))
                 (:file "suites" :depends-on ("packages"))
                 (:module "ansi"
                  :depends-on ("suites" "rt" "packages")
                  :components ((:file "aux")
                               (:file "subtypep-array"
                                :depends-on ("aux"))
                               (:file "subtypep-complex"
                                :depends-on ("aux"))
                               (:file "subtypep-cons"
                                :depends-on ("aux"))
                               (:file "subtypep-eql"
                                :depends-on ("aux"))
                               (:file "subtypep-float"
                                :depends-on ("aux"))
                               (:file "subtypep-function"
                                :depends-on ("aux"))
                               (:file "subtypep-integer"
                                :depends-on ("aux"))
                               (:file "subtypep-member"
                                :depends-on ("aux"))
                               (:file "subtypep-rational"
                                :depends-on ("aux"))
                               (:file "subtypep-real"
                                :depends-on ("aux"))
                               (:file "subtypep" :depends-on ("aux"))))))))
