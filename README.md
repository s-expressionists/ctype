This system is an implementation of the Common Lisp type system; particularly `cl:typep` and `cl:subtypep`.

The function `specifier-ctype` takes a type specifier and environment as input, and returns a "ctype": a reified representation of a type, independent of any environment. Ctypes are a precise reflection of their input specifier, i.e. information independent of the environment is not lost. They are however simplified as much as possible, so they will not reflect redundant information in the specifier. For example, `(and list cons)` and `cons` are interpreted as the same ctype.

The `ctypep` and `subctypep` functions implement `cl:typep` and `cl:subtypep`, except that they take ctype objects as arguments, rather than type specifiers. Then the CL functions could be defined as

```lisp
(defun typep (object type-specifier &optional environment)
  (ctypep object (specifier-ctype type-specifier environment)))

(defun subtypep (type-specifier-1 type-specifier-2 &optional environment)
  (subctypep (specifier-ctype type-specifier-1 environment)
             (specifier-ctype type-specifier-2 environment)))
```

The functions `negate`, `disjoin`, and `conjoin` can be used to compute functions of ctypes. They are analogous to the compound type specifiers `not`, `or`, and `and` respectively.

The functions `top` and `bot` return the top ctype and bottom ctype (`t` and `nil`), respectively. `top-p` and `bot-p` determine whether a given ctype is the top or the bottom ctype, respectively.

# Configuration

This system is intended for use in an implementation of `typep` and `subtypep`, and so does not use `cl:typep` or `cl:subtypep` at all. Unfortunately, not all aspects of the type system on a given Lisp system are determinable with standard means without using `typep` and `subtypep`, and must be manually configured. The configuration of the system is in config.lisp and consists of

 * `ratiop`: a function that returns true iff its one argument evaluates to a ratio.
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

# Classes

Ctypes are of class `ctype`. Various subclasses of `ctype` implement kinds of types in the CL type system. The following subclasses are defined by the system:

 * `cclass`: a ctype representing a class. The class may be read with the `cclass-class` function.
 * `negation`: The negation of its `negation-ctype`.
 * `conjunction`/`disjunction`: Represents uses of the `and`/`or` (resp.) type specifier that could not be further simplified. `junction-ctypes` returns a list of the ctypes it is a con/disjunction of.
 * `ccons`: A cons type. `ccons-car` and `ccons-cdr` read the `car` and `cdr` types respectively.
 * `range`: A range of real numbers. `range-kind` is one of `integer`, `ratio`, `short-float`, `single-float`, `double-float`, or `long-float`. `range-low`, `range-low-exclusive-p`, `range-high`, and `range-high-exclusive-p` read the properties of the range.
 * `ccomplex`: A `complex` type. `ccomplex-ucpt` reads the upgraded complex part type, which is either the symbol `cl:*`, or something returned by `cl:upgraded-complex-part-type`.
 * `cmember`: A `member` or `eql` type. `cmember-members` returns a list of the objects of the type.
 * `carray`: An array type. `carray-simplicity` reads `:simple` or `:complex` accordingly; array types including both are represented as disjunctions. `carray-uaet` reads the upgraded array element type. `carray-dims` reads the dimension specification, which is a `dimension-spec` as accepted by the `cl:array` compound type specifier.
 * `charset`: A subtype of `character`. `charset-pairs` reads the description of the codes included, which is as described above for `+standard-charset+` in the configuration section.
 * `cvalues`: A `values` type.
 * `cfunction`: A `function` type.
 * `csatisfies`: A `satisfies` type.

Additional classes may be defined by the programmer.

# Generic functions

Methods on `ctypep` and `subctypep` must be implemented for subclasses of `ctype` in order for those functions to work correctly.

Methods on `subctypep` should return the result of `(call-next-method)` if they cannot determine a conclusive answer, i.e. if they would return `(values nil nil)`. This ensures that all applicable methods can have a shot at giving a definitive answer.

A method on `unparse` must be defined for ctypes to print correctly. `unparse` should return a type specifier that could specify the given ctype. This is only used for display purposes, so it doesn't have strict requirements.

The additional generic functions `disjointp`, `negate`, `conjoin/2`, `disjoin/2`, and `subtract` may also need methods in order for `subctypep` and `specifier-ctype` to work correctly. Particularly, if the conjunction of two types is recognizably (with `subctypep`) the bottom type, `conjoin/2` must return `(bot)` and `disjointp` must return definite truth, and similarly with disjunction and `(top)`.

 * `disjointp` has the same return value convention as `subtypep`, and similarly, methods should use `call-next-method` if the answer cannot be determined. `disjointp` can be used to determine if two ctypes are completely disjoint: `(disjointp (specifier-ctype x) (specifier-ctype y))` is equivalent to `(subctypep (conjoin (specifier-ctype x) (specifier-ctype y)) (specifier-ctype nil))`.
 * `negate` computes the negation of a ctype, i.e. if a ctype is specified by `x`, `(negate that-ctype)` is specified by `(not x)`. The default method makes a `negation` ctype. These ctypes do not provide enough information for all functions to work well, e.g. they may result in `nil nil` answers from `subctypep`. As such, if the negation of a type can be expressed in a better way, a specializing method on `negate` should be defined.
 * `conjoin/2` and `disjoin/2` are the two-argument functions underlying `conjoin` and `disjoin` respectively. If no special behavior is defined, `conjoin` and `disjoin` will create `conjunction` and `disjunction` types, which do not always provide enough information for precise answers from `subctypep`.
 * `subtract`, given ctypes specified by `x` and `y`, may compute the ctype specified by `(and x (not y))`. If no special behavior is defined with a method, a `conjunction` ctype will be made, which is suboptimal.
