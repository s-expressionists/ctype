Ctype can be divided into a few subsystems:

* The _parsing_ subsystem parses type specifiers into ctype objects.
* The _relations_ interface computes relations between types.
* The _operations_ interface constructs new type objects from old ones through set operations.
* The _objects_ system defines the various kinds of ctypes, and accessors for getting their particular properties.

These are not (at least at the moment) actual distinct ASDF systems or packages. The _objects_ system does not have dependencies, and is entirely in classes.lisp. The _parsing_ system depends on all of the other systems, and is defined entirely in parse.lisp. The _relations_ and _operations_ interfaces are defined in generic-functions.lisp, and methods are defined throughout most of the rest of the system. The _operations_ system depends on the _relations_ system and not vice versa; this is to make the implementation clearer, and so that queries on types don't cons up new types.

If ctype is just to be used to implement the Lisp type system, the entry points to the library are just `specifier-ctype`, `ctypep`, and `subctypep`. The first of these is part of the parser and the latter two are relations. More sophisticated usage my implicate the operations interface directly. Anyone defining their own kinds of type will need to define methods in several places.

# Parsing

## Definitions

[Function]

**specifier-ctype** *type-specifier* `&optional` *environment* => *ctype*

Parses a type specifier to a ctype in the given environment. The default environment is `nil`, representing the current global environment. The environment is used to get information about type macros defined with `deftype`.

---

[Function]

**extended-specifier-ctype** *type-specifier* `&optional` *environment* => *ctype*

Parses a type specifier to a ctype in the given environment. Parts of the type-specifier might be using extended types. The default environment is `nil`, representing the current global environment. The environment is used to get information about type macros defined with `deftype`.

---

[Macro]

**define-extended-type** *name* *lambda-list* `&key` *documentation* *simple* *extended* => *name*

Defines an extended type specifier called name. If it is parsed using `specifier-ctype` or some other non-extended parsing facility, the simple forms are used to create a more primitive type specifier. If it is parsed using `extended-type-specifier`, the extended forms are used to create a ctype.  Both the simple and extended forms are required.

---

# Relations

## Definitions

[Generic Function]

**ctypep** *object* *ctype* => *generalized-boolean*

The ctype analogy to `cl:typep`. It has the same semantics, except that it takes a ctype object rather than a type specifier, and because ctypes are independent of the environment there is no need for an environment parameter.

No default method is provided; programmers implementing their own kinds of ctype should define appropriate methods on **ctypep**. Without such methods, library functions may signal errors, including other library functions, as operations on `cl:member` types need to test types of objects.

---

[Generic Function]

**subctypep** *ctype1* *ctype2* => *subtypep-p*, *valid-p*

The ctype analogy to `cl:subtypep`. It has the same semantics, except that it takes ctype objects rather than type specifiers, and because ctypes are independent of the environment there is no need for an environment parameter.

A default method is provided that returns `nil nil`, i.e. uncertainty.

---

[Generic Function]

**ctype=** *ctype1* *ctype2* => *type-equal-p*, *valid-p*

Determines whether the two ctypes are identical, i.e. represent the same set of objects.

A default method is provided that calls *subctypep* in both directions. If each is a subtype of the other, the default method returns true; if at least one is not a subtype of the other, the default method returns false; and otherwise it returns uncertainty.

---

[Generic Functions]

**disjointp**, **conjointp** *ctype1* *ctype2* => *joint-p*, *valid-p*

Determines whether two ctypes are disjoint or conjoint. Two ctypes are disjoint if their conjunction is bottom, i.e. they share no elements. Two ctypes are conjoint if the disjunction is top, i.e. between them they include all possible objects.

A default method is provided that returns `nil nil`, i.e. uncertainty.

Because programmers can define new classes, types are essentially never conjoint unless one is a negation, and as described under "Custom methods" below, programmers do not need to define this behavior themselves. Usually the only **conjointp** method necessary is one to define that two ctypes of whatever kind are never conjoint. For example, the built in cons ctypes, array ctypes, etc. have **conjointp** methods like this, as well as methods to indicate that e.g. a cons ctype and array ctype are never conjoint.

---

[Generic Function]

**cofinitep** *ctype* => *cofinite-p*, *valid-p*

Determines whether the complement/negation of a given ctype is finite. This is used to resolve questions like `(subtypep '(not X) '(member ...))`: if X is cofinite, this is surely false even if nothing else is known about X.

A default method is provided that returns `nil nil`, i.e. uncertainty.

Most kinds of ctype are always cofinite.

---

## Laws

Several relations imply other relations. We have the following logical laws, given any ctypes `c1`, `c2`, and `c3`, and understanding that the possible values for a relation are true, false, and unknown (so e.g. "not true" means "false or unknown"):

* If `(subctypep c1 c2)` and `(subctypep c2 c3)` are true, `(subtypep c1 c3)` is not false.
* If `(subctypep c1 c2)` is true and `(subctypep c2 c1)` is true, `(ctype= c1 c2)` is not false.
* If `(ctype= c1 c2)` is true, `(subctypep c1 c2)` and `(subctypep c2 c1)` are not false.
* If `(ctype= c1 c2)` and `(ctype= c2 c3)` are true, `(ctype= c1 c3)` is not false.
* If `(subctypep c1 c2)` is false or `(subctypep c2 c1)` is false, `(ctype= c1 c2)` is not true.
* If `(ctype= c1 c2)` is false, `(subctypep c1 c2)` and `(subctypep c2 c1)` are not both true.
* If `(disjointp c1 c2)` is true, `(subtypep c1 c2)` and `(subtypep c2 c1)` are not, unless one or both are the bottom type.
* If `(conjointp c1 c2)` is true, `(subtypep c1 c2)` and `(subtypep c2 c1)` are not, unless one or both are the top type.

## Custom methods

Custom methods on these generic functions should be written carefully to avoid infinite regress. In general, they should call each other directly on their arguments as little as possible to avoid accidents.

The functions **subctypep**, **ctype=**, **disjointp**, etc. may return unsure results. These results mean what `cl:subtypep`'s result values do: Two true values mean the relation definitely holds, false and true mean it definitely doesn't, and false and false mean it cannot be determined. Users of these functions, including custom methods on ctype functions, should be prepared to deal with all three possible results. Unsure results may always be returned, and will be dealt with appropriately by other ctype functions.

If a given method cannot determine that the relation is true or false, it must return `(values nil nil)`. A custom (unexported) method combination takes care of combining results from multiple methods; each method should work totally independently, and only return a sure result if it's definitely correct. Similar to a built-in short form method combination, `call-next-method` is not available in primary methods.

Custom methods should *not* define generic methods, i.e. unspecialized beyond **ctype**, because ctype kinds may overlap with each other. For example, a custom "proper list of X" ctype would overlap with the existing cons ctypes. If there was a method defining cons ctypes to not be subtypes of unknown ctypes, inconsistency would result unless everything was overriden completely correctly; because there isn't, the built in method just returns uncertainty, which is always consistent.

User methods on these functions must preserve the above laws in order for the system to work correctly.

Unless otherwise noted, all of these functions have generic methods on set-theoretic kinds, i.e. conjunctions, disjunctions, negation, bottom, and top. Custom ctype kinds do not need to define methods for relations with these kinds of ctypes. However, methods are *not* especially provided that will apply between a custom ctype and other built in kinds of ctype (i.e. standard types), so programmers should define such methods.

User methods on _relations_ functions should not call _operations_ functions directly on their arguments. This is because _operations_ functions are permitted to call _relations_ functions on their arguments, so infinite recursion could result. Additionally, this arrangement keeps as much actual logic as possible in the non-consing, conceptually pure _relations_ functions rather than the _operations_ functions.

# Operations

## Defintions

[Functions]

**disjoin**, **conjoin** `&rest` *ctypes* => *ctype*

Computes the disjunction or conjunction of the given ctypes, respectively. The disjunction of ctypes is the ctype representing the disjunction of the sets of objects represented by the ctypes, and conjunction is analogous.

If no ctypes are provided, **disjoin** returns the bottom ctype, and **conjoin** the top ctype, as per basic mathematics. If only one ctype is provided, it is returned. Otherwise, **disjoin** uses **disjoin/2** and **conjoin** uses **conjoin/2** to simplify the result as much as possible, and otherwise constructs generic disjunction or conjunction types. The order **conjoin/2** or **disjoin/2** are called in, and how often (or if they are called at all), is unspecified.

---

[Generic Function]

**negate** *ctype* => *ctype*

Computes the negation/complement of the given ctype. This is the ctype representing the set of all objects that are not included in the set of objects represented by the given ctype.

A default method is provided that returns a generic negation type. This function uses the standard method combination, and `call-next-method` should be used by custom methods if no simplification is possible.

---

[Generic Functions]

**disjoin/2**, **conjoin/2** *ctype1* *ctype2* => *maybe-ctype*

The generic functions **disjoin/2** and **conjoin/2** are not intended to be called by programmers. Programmers may define methods for them. They are called by **disjoin** or **conjoin** respectively, as described for those functions.

**disjoin/2** and **conjoin/2** attempt to compute a simple disjunction or conjunction (respectively) of the given ctypes. If no simplification is possible, they return `nil`, indicating that a generic disjunction or conjunction should be constructed instead.

Default methods are provided that check for conjointness/disjointness (respectively) and subtype relations, and otherwise return `nil`. These functions use a custom unexported method combination; `call-next-method` is not available in primary methods, and methods should work independently.

---

[Generic Function]

**subtract** *ctype1* *ctype2* => *maybe-ctype*

The generic function **subtract** is not intended to be called by programmers. Programmers may define methods for it. **subtract** is called by the system when computing certain conjunctions.

**subtract** attempts to compute a simple difference of `ctype1` and `ctype2`, i.e. the type that includes all of `ctype1`'s objects but none of `ctype2`'s. If no simplification is possible, it returns `nil`, indicating that a generic conjunction should be constructed instead.

Default methods are provided that check for the special cases of disjointness and `ctype` being a subtype of `ctype2`, and otherwise return `nil`. This function uses a custom unexported method combination; `call-next-method` is not available in primary methods, and methods should work independently.

---

## Laws

Similarly to the _relations_, many set-theoretic laws are in place. Given ctypes `c1` and `c2`, their conjunction `C` and disjunction `D`, and the negation of `c1`, `c1p`:

* `(subtypep C c1)` and `(subtypep C c2)` are not false.
* `(subtypep c1 D)` and `(subtypep c2 D)` are not false.
* `(subtypep c1 c1p)` is not true unless c1 is bottom.
* `(subtypep c1p c1)` is not true unless c1 is top.
* `(disjointp c1 c1p)` and `(conjointp c1 c1p)` are not false.

## Custom methods

Custom methods on these generic functions should be written carefully to avoid infinite regress. In general, they should call each other directly on their arguments as little as possible to avoid accidents.

User methods on these functions must preserve the above laws in order for the system to work correctly.

What counts as "simplification" is sometimes not obvious. The general rule is that regardless of what simplifications are applied or not applied, relations between constructed types must work correctly; i.e. they may return unknown, but they may not return a "sure" incorrect answer. Simplification should ideally result in a ctype that can be related more precisely.

Unless otherwise noted, all of these functions have generic methods on set-theoretic kinds, i.e. conjunctions, disjunctions, negation, bottom, and top. Custom ctype kinds do not need to define methods for relations with these kinds of ctypes. However, methods are *not* especially provided that will apply between a custom ctype and other built in kinds of ctype (i.e. standard types), so programmers should define such methods. In particular, the system does not assume that new kinds are disjoint from existing ones.

# Objects

TODO
