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

;; the idea of this is that it signals errors even on safety 0
;; but we're not integrated with the compiler anyway.
(defmacro signals-error-always (form condition-type)
  `(signals-error ,form ,condition-type))
