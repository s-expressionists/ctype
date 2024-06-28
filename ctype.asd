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
      ((:file "list-of")
       (:file "array-of")
       (:file "hash-table-of")))))))

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

(defsystem :ctype/test
  :author "Bike <aesthaer@gmail.com>"
  :maintainer "Bike <aeshtaer@gmail.com>"
  :depends-on (:ctype :fiveam)
  :components
  ((:module "test"
    :components ((:file "packages")
                 (:file "framework" :depends-on ("packages"))
                 (:file "integer" :depends-on ("framework" "packages"))
                 (:file "member" :depends-on ("framework" "packages"))
                 (:file "cons" :depends-on ("framework" "packages"))
                 (:file "array" :depends-on ("framework" "packages"))))))
