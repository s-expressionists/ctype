# Modular arithmetic

The `mod.lisp` file is a self contained implementation of integer residues as types. A `congruence` ctype represents the set of all integers that have some value mod the congruence's modulus. A congruence object can represent more than one residue with the same modulus. Each residue is stored as a bit in a bitfield.

For example, the set of all `evenp` integers is `(ctype.ext.mod:congruence 2 #b01)`. The least significant bit of `#b01` is one, so the class includes 0 mod 2. The next bit is zero, so the class does not include 1 mod 2. That, the set of all `oddp` integers, would be `(congruence 2 #b10)`. The usual ctype operators work on these types, so for example `conjoin`ing them gets you the empty type and `disjoin`ing them gets you the type of all integers. The constructor and methods automatically detect and reduce these sorts of degeneracies.

The type of all integers that are not a multiple of three (i.e. that are 1 mod 3 or 2 mod 3) would be `(congruence 3 #b110)`. This too can be conjoined and disjoined at will, e.g. `(conjoin (congruence 3 #b110) (congruence 2 #b01))` => 2 or 4 mod 6, while `(disjoin (congruence 3 #b110) (congruence 2 #b01))` => 0, 1, 2, 4, or 5 mod 6. The conjunctions and disjunctions of congruences are always either congruences, `integer`, or `nil`.

# Indefinite-length lists

The `list-of.lisp` file is a self contained implementation of the type of lists of some element type. This type cannot be expressed in the Common Lisp type system but is sometimes desired. In a little more detail, `(list-of x)` can be expressed recursively as being the object `nil` plus all objects of type `(cons x (list-of x))`. This includes circular lists, but not dotted lists.

# Type-level functions

The largest extension so far is for tfuns, short for "type-level functions". This system can determine upper bounds for the result type of function calls, given knowledge of the function being called and upper bounds on the types of its arguments.

It can be loaded as the `ctype-tfun` ASDF system. This system has ctype proper as well as alexandria as dependencies.

The primary entry points are `derive-call` and `derive-multiple-value-call`. `derive-call` accepts a tfun (or other ctypes, but this will be less useful) as well as single value ctypes for the arguments to the call. For example, in order to approximate the type of `(+ (the (single-float 3.9 48.2) x) (the (rational 8 32) y))`, you would do

```
(ctype.ext.tfun:derive-call
 (ctype.ext.tfun:find-tfun '+)
 (ctype:specifier-ctype '(single-float 3.9 48.2))
 (ctype:specifier-ctype '(rational 8 32)))
=> #<CTYPE:CVALUES (VALUES (SINGLE-FLOAT 11.9 80.2) &REST NIL)>
```

This indicates that exactly one value is returned, a `(single-float 11.9 80.2)`.

`find-tfun` returns a tfun for the given function name. It accepts an `errorp` argument that works like that of `cl:find-class`. If there is no tfun, and errorp is false, a generic function ctype will be returned instead.

As you may have gathered, tfuns are themselves ctypes. This is intended to facilitate static analyses that track functions as variables in Lisp programs. As ctypes, tfuns will conjoin/disjoin/etc correctly with non-function ctypes, and will give usually subpar but correct results with function ctypes.

## Available functions

Currently the set of standard functions with tfuns defined is quite limited, but it is intended to expand:

 * Data and control flow: `funcall`, `apply`, `not`, `eql`, `identity`, `values`, `values-list`
 * Numbers: `=`, `<`, `>`, `<=`, `>=`, `+`, `-`, `*`, `/`, `exp`, `ash`, `integer-length`, `logcount`
 * Conses: `car`, `cdr`, `caar` etc.
 * Arrays: `aref`, `row-major-aref`

## Extension & how it works

New tfuns can be defined with the `define-tfun` operator. This will define a (Lisp) function that accepts a values ctype as arguments, and should return a values ctype for the return values. A lambda list can be provided to make parsing the values ctype easy, so that for functions not involving &rest the fact the input type is a values ctype can be safely ignored.

Tfuns operate on values ctypes for the sake of coherence. Lisp functions can be conceived of as taking and returning exactly one "value" - a multiple value structure. For a conventional call, the input values type has all required values, no optional values, and a &rest type of bottom, meaning that the number of arguments is exactly known. For a `multiple-value-call` or `apply`, the values type may be more complicated. Arranging things this way allows deriver functions to work on calls with unknown argument counts.

## Caveats

Unlike the ctype system proper, tfuns only provide upper bounds. This is probably fine for most applications (e.g. type inference for optimization purposes), but should be kept in mind. As an example, `(derive-call (find-tfun 'ash) (specifier-ctype '(eql 1)) (specifier-ctype '(integer 0 4)))` gives `(integer 1 16)`, but this is too broad: it includes e.g. 3, which `(ash 1 anything)` will obviously never be. The exact answer would be `(member 1 2 4 8 16)`.

The system assumes that if argument restrictions are violated, functions will return the bottom type (i.e. signal an error or otherwise transfer control). This is true in most Lisp implementations, but is not always technically defined to be the case by the standard. A user concerned with strict conformity should be careful. In the future, this is intended to be configurable or otherwise possible to deal with. Making this assumption is important to getting decent results without too much fussing over exact correctness.
