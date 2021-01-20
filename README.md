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

# Classes

Ctypes are of class `ctype`. Various subclasses of `ctype` implement kinds of types in the CL type system. Additional classes may be defined by the programmer.

# Generic functions

Methods on `ctypep` and `subctypep` must be implemented for subclasses of `ctype` in order for those functions to work correctly.

Methods on `subctypep` should return the result of `(call-next-method)` if they cannot determine a conclusive answer, i.e. if they would return `(values nil nil)`. This ensures that all applicable methods can have a shot at giving a definitive answer.

A method on `unparse` must be defined for ctypes to print correctly. `unparse` should return a type specifier that could specify the given ctype. This is only used for display purposes, so it doesn't have strict requirements.

The additional generic functions `disjointp`, `negate`, `conjoin/2`, `disjoin/2`, and `subtract` may also need methods in order for `subctypep` and `specifier-ctype` to work correctly. Particularly, if the conjunction of two types is recognizably (with `subctypep`) the bottom type, `conjoin/2` must return `(bot)` and `disjointp` must return definite truth, and similarly with disjunction and `(top)`.

 * `disjointp` has the same return value convention as `subtypep`, and similarly, methods should use `call-next-method` if the answer cannot be determined. `disjointp` can be used to determine if two ctypes are completely disjoint: `(disjointp (specifier-ctype x) (specifier-ctype y))` is equivalent to `(subctypep (conjoin (specifier-ctype x) (specifier-ctype y)) (specifier-ctype nil))`.
 * `negate` computes the negation of a ctype, i.e. if a ctype is specified by `x`, `(negate that-ctype)` is specified by `(not x)`. The default method makes a `negation` ctype. These ctypes do not provide enough information for all functions to work well, e.g. they may result in `nil nil` answers from `subctypep`. As such, if the negation of a type can be expressed in a better way, a specializing method on `negate` should be defined.
 * `conjoin/2` and `disjoin/2` are the two-argument functions underlying `conjoin` and `disjoin` respectively. If no special behavior is defined, `conjoin` and `disjoin` will create `conjunction` and `disjunction` types, which do not always provide enough information for precise answers from `subctypep`.
 * `subtract`, given ctypes specified by `x` and `y`, may compute the ctype specified by `(and x (not y))`. If no special behavior is defined with a method, a `conjunction` ctype will be made, which is suboptimal.
