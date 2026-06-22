(in-package #:ctype.test)

;;; entry points
(defun run (*client*) (5am:run 'ctype))

(defun run! (*client*) (5am:run! 'ctype))

;;; ansi-tests compatibility
(defmacro deftest (name form &rest expected)
  `(5am:test ,name
             (5am:is (equal '(,@expected)
                            (multiple-value-list ,form)))))

(defmacro signals-error (form condition-type)
  `(5am:signals ,condition-type ,form))
