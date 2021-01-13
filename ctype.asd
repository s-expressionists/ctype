(defsystem :ctype
  :depends-on ()
  :components
  ((:file "packages")
   (:file "config" :depends-on ("packages"))
   (:file "classes" :depends-on ("packages"))
   (:file "create" :depends-on ("classes" "packages"))
   (:file "generic-functions" :depends-on ("create" "classes" "packages"))
   (:file "cclass" :depends-on ("generic-functions" "classes" "config"
                                                    "packages"))
   (:file "negation" :depends-on ("generic-functions" "create" "classes"
                                                      "packages"))
   (:file "conjunction" :depends-on ("generic-functions" "create" "classes"
                                                         "packages"))
   (:file "disjunction" :depends-on ("generic-functions" "create" "classes"
                                                         "packages"))
   (:file "ccons" :depends-on ("generic-functions" "create" "classes"
                                                   "packages"))
   (:file "range" :depends-on ("generic-functions" "create" "classes"
                                                   "config" "packages"))
   (:file "ccomplex" :depends-on ("generic-functions" "create" "classes"
                                                      "config" "packages"))
   (:file "cmember" :depends-on ("generic-functions" "create" "classes"
                                                     "packages"))
   (:file "carray" :depends-on ("generic-functions" "create" "classes"
                                                    "packages"))
   (:file "csatisfies" :depends-on ("generic-functions" "create" "classes"
                                                        "packages"))
   (:file "pairwise" :depends-on ("generic-functions" "create" "classes"
                                                      "packages"))))
