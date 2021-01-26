(in-package #:ctype)

;;;; TODO: Thread safety

(defmacro cached ((cache &key (nvals 1) (cache-bits '10) (mix 'logxor)
                           (compare 'eql) (hasher 'sxhash) (evict 't))
                  (&rest keys) &body computer)
  (let* ((nkeys (length keys))
         (gkeys (loop repeat nkeys collect (gensym "KEY")))
         (gvals (loop repeat nvals collect (gensym "VAL")))
         (ghash (gensym "HASH")) (gcache (gensym "CACHE"))
         (gentry (gensym "ENTRY")))
    `(let* ((,gcache ,cache)
            ,@(mapcar #'list gkeys keys)
            (,ghash (ldb (byte ,cache-bits 0)
                         (,mix ,@(loop for gkey in gkeys
                                       collect `(,hasher ,gkey)))))
            (,gentry (svref ,gcache ,ghash)))
       (if (and ,gentry
                ;; Hit an entry; see if the keys match
                ,@(loop for gkey in gkeys
                        for i from 0
                        collect `(,compare ,gkey (svref ,gentry ,i))))
           ;; Valid. Return the vals.
           (values ,@(loop for i from nkeys below (+ nkeys nvals)
                           collect `(svref ,gentry ,i)))
           ;; Miss or collision. Bummer. Since this is the slow path we
           ;; redundantly check ,gentry again, who cares?
           (multiple-value-bind (,@gvals)
               (progn ,@computer)
             (,@(if evict '(progn) `(unless ,gentry))
              ;; gensym not required since COMPUTER is the only user code
              (let ((new-entry (make-array ,(+ nkeys nvals))))
                (setf ,@(loop for gkey in gkeys
                              for i from 0
                              append `((svref new-entry ,i) ,gkey))
                      ,@(loop for gval in gvals
                              for i from nkeys
                              append `((svref new-entry ,i) ,gval)))
                (setf (svref ,gcache ,ghash) new-entry)))
             (values ,@gvals))))))
