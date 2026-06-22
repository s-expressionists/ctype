(in-package #:ctype.ext.tfun)

(defun single-value (ctype)
  (cvalues (list ctype) nil (bot)))

(defun constant-type-p (ctype)
  (and (typep ctype 'cmember)
       (= (length (cmember-members ctype)) 1)))

(defun constant-type-value (ctype)
  (first (cmember-members ctype)))

;;; Convenience macros to define some methods common to most generic function
;;; deriver auxiliaries throughout this system.
;;; Note that we don't do negations by default, since negation is not
;;; preserved by all (or indeed most, far as I can tell) functions.

(defun cjaux (fname params jclass funform)
  (let ((type (gensym "TYPE"))
        (client (gensym "CLIENT")))
    `(progn
       ,@(loop with gclasses = (make-list (length params)
                                          :initial-element 'ctype)
               for param in params
               for i from 0
               for classes = (let ((g (copy-list gclasses)))
                               (setf (nth i g) jclass)
                               g)
               for args = (let ((g (copy-list params)))
                            (setf (nth i g) type)
                            g)
               collect `(defmethod ,fname (,client
                                           ,@(mapcar #'list params classes))
                          (apply ,funform ,client
                                 (mapcar (lambda (,type) (,fname ,@args))
                                         (junction-ctypes ,param))))))))

(defmacro defconjunctions (fname (&rest params))
  (cjaux fname params 'conjunction '#'conjoin))
(defmacro defdisjunctions (fname (&rest params))
  (cjaux fname params 'disjunction '#'disjoin))

(defmacro defdefaults (fname (client &rest params)
                       &optional (max-result '(top) mrp))
  `(progn
     (defmethod ,fname (,client ,@(loop for param in params
                                       collect `(,param ctype)))
       (declare (ignorable ,client))
       ,max-result)
     ,@(when mrp
         `((defmethod ,fname :around (,client
                                      ,@(loop for param in params
                                              collect `(,param ctype)))
             (conjoin ,client (call-next-method) ,max-result))))
     (defconjunctions ,fname (,@params))
     (defdisjunctions ,fname (,@params))))
