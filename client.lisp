(in-package #:ctype)

(defgeneric range-kindp (client object kind))
(defgeneric most-positive-fixnum (client))
(defgeneric most-negative-fixnum (client))
(defgeneric reduce-float-type (client type))
(defgeneric distinct-float-types (client))
(defgeneric distinct-zeroes-p (client type))
(defgeneric complex-ucptp (client object upgraded-complex-part-type))
(defgeneric upgraded-complex-part-type (client type-specifier
					&optional environment))

(defgeneric simple-array-p (client object))
(defgeneric complex-arrays-distinct-p (client))
(defgeneric string-uaets (client))
(defgeneric upgraded-array-element-type (client type-specifier
					 &optional environment))

(defgeneric find-class (client class-name &optional errorp env))
(defgeneric subclassp (client class1 class2))
(defgeneric class-alias (client class-name))

(defgeneric standard-charset-pairs (client))
(defgeneric base-charset-pairs (client))
(defgeneric char-code-limit (client))

(defgeneric sfdefinition (client function-name))

(defgeneric typexpand (client specifier environment))
