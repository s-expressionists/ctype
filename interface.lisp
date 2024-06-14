(in-package #:ctype-extrinsic)

(defvar *client* nil)

(defun typep (object type-specifier &optional environment)
  (ctype:ctypep *client* object
                (ctype:specifier-ctype
                 *client* type-specifier environment)))

(defun subtypep (type-specifier1 type-specifier2 &optional environment)
  (ctype:subctypep
   *client*
   (ctype:specifier-ctype *client* type-specifier1 environment)
   (ctype:specifier-ctype *client* type-specifier2 environment)))

(defun specifier-ctype (type-specifier &optional environment)
  (ctype:specifier-ctype *client* type-specifier environment))
