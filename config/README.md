This system is intended for use in an implementation of `typep` and `subtypep`, and so does not use `cl:typep` or `cl:subtypep` at all. Unfortunately, not all aspects of the type system on a given Lisp system are determinable with standard means without using `typep` and `subtypep`, and must be manually configured per implementation. A config file for an implementation must define the following:

 * `ratiop`: a function that returns true iff its one argument is a ratio.
 * `+floats+`: An alist. Each `car` is one of `short-float`, `single-float`, `double-float`, or `long-float`. Each `cdr` is a symbol naming an operator that returns true iff its one argument is of the corresponding type. If the system merges one or more float types, only the float types it actually has should be defined, as explained in the CLHS page on these types.
 * `+standard-charset+`: A list of pairs of character codes. Each pair represents an inclusive range. These ranges must not overlap or touch. All character codes for characters of type `standard-char`, and no others, should be included. For example, as mentioned in the file implementations where character codes are as in ASCII will have a `+standard-charset+` of `((10 . 10) (32 . 126))`.
 * `+base-charset+`: As `+standard-charset+`, but all and only codes for characters of type `base-char` should be included.
 * `+string-uaets+`: A proper list of upgraded array element types. This should be a complete list of all upgraded array element types that are a subtype of `character`. This list is used to parse `string` and `simple-string` types.
 * `+complex-arrays-exist-p+`: Must be true iff the implementation has complex arrays, i.e. arrays that are not of type `simple-array`.
 * `simple-array-p`: A function that returns true iff its one argument is a simple array.
 * `+class-aliases+`: A list. Each element consists of `(class-name type-specifier)`. When the `specifier-ctype` is given the `class-name`, or a class of that name, it will treat it as though it had seen the type specifier instead. This is primarily intended for classes that are subclasses of `array`.
 * `subclassp`: A function of two classes, that returns true iff the first is a subclass of the second.
 * `typexpand`: A function of a type specifier and an environment, that performs any `deftype` expansions, analogous to `macroexpand`.
 * `complex-ucptp`: A macro that expands into code that returns ture iff the object its first form evaluates into, which is a `complex`, has the given upgraded complex part type.

Additionally, there is a constraint that `char-code-limit` is the actual upper limit of character codes (i.e. there are no positive integers less than `char-code-limit` that are not character codes), and that `cl:upgraded-array-element-type` and `cl:upgraded-complex-part-type` return objects that can be sensibly compared with `cl:equal`.
