# Format #

`Format` is a type for specifying how a certain data type is formatted.
`Format` is a unary type function, so a `Format t` specifies a format for `t`s.
`Format` can be used instead of a separate pretty-printer and parser.
This eliminates one of two places to implement changes.


## Semantics ##

This specification can be interpreted both for reading and writing that format.
`forite` and `foread` implement these semantics, open to optimization:
```
Forite.forite :: Format t -> t -> String
Foread.foread :: Format t -> String -> Maybe t
```
Note that unlike parsers, `foread` allows only one possible interpretation.
Also, it doesn't discard remaining input, but requires that there is none.

`formeq` helps by expressing some constructors of `Format`s in terms of others.

`Format` is closed, but additional constructors can be emulated via pattern synonyms.

Similar to `show` and `read`, `forite` and `foread` guarantee uniqueness:
* unique interpretation: `foread f . forite f == Just`
* unique representation: `let Just x = foread f s in forite f x == s`

### Uniqueness of Interpretation ###

For the former to hold, the contained functions need to be inverses of each other.
This is true for `FAtom`s, `FTrans`s but also `FLdbl` and `FEnum`, as well as
 the functions with multi-part inverses in `F2,F3,F4,F2ple,F3ple,FWrap,FWraple`.

### Uniqueness of Representation ###

For the latter to hold, TODO has to be true.
Also, `FAlt` by definition breaks this property, providing two equally valid formats. TODO .


## Construction ##

`Format` constructors (or "combinators") work similarly to parser combinators.
Unfortunately `Format` is no `Functor` because it needs an inverse pair of functions:
```
bidirectional_fmap :: (a -> b) -> (b -> a) -> Format a -> Format b
bidirectional_fmap = F1
```
So the `do`-notation for monadic computations is unavailable, unlike for parsers.

In the following, details on some interesting constructors are explained.

### Constants ###

TODO incorporate changes:
* FConstext now a pattern,
* it can fail to parse because it uses FConst,
* which now crucially checks for string equality)

`FConstext :: String -> Format a` is a write-only constructor.
At first it seems mainly useful for completing theoretical constructs.
But it also helps in formats where one value relates to multiple places in the text,
 in some of which not its entire information is represented.
It's often more convenient to:
* write distributing the entire value, with `FConstext` to collapse certain cases
* read ignoring the result of the information-losing places
than to establish a split representation just for a format.
(`FConstext` necessitates ignoring what's read from the respective place completely,
 since it may be `undefined` and even the simplest check on it will fail.)

`FConst` loses information in both directions, but its ignorance can be bliss.
It can avoid a `Eq` constraint on some container `f t`s inner type `t`,
 when a special case for it being empty, `null`, `(is)Nothing` or so is needed.
`FXcpt` would ask for `Eq (f t)`, and so would `FNtry` inside a `FCond`.
That would propagate to the contained type even though it's never compared.
A `FConst` inside a corresponding `FCond` is sure to never actually lose information,
 but by skipping any equality test, keeps the signature of where it's used clean.
Specifically, those places are the `FMaybe` and `FEnum` cases (see `formeq`).

### Injection ###
Consider the constructor:
`FInJect :: (Eq e) => Format (e,t) -> e -> Format t`
. It represents fixing one component of a product type to a particular value.
That value can be said to be "injected" into the `Format`.
Conversely, one might want to "extract" a value from a product type to be formatted:
`FXtract :: (e -> Format t) -> Format (e,t)`
Here, the `e` could appear however it pleases in the `Format t`,
 with no concern for which proper `Format` combinators to represent that.
Unfortunately, this construction can't be interpreted by `foread`,
 because the only argument to `FXtract` is an opaque function.
We can't expect to know in the general case how to guess that `e`.

### Alternative ###

TODO , see ## Group theory?

### Fork ###
Judging by its type signature and name,
`FFork :: Format a -> Format a -> Format a`
may be expected to just be a special case of `F2` with the two types being equal,
 to each other and to the combined type, so the "separation" are simply `id`s:
`formeq (FFork f0 f1) = F2 ? id f0 id f1`
But `F2` applies the combining function applicatively over the read elements.
This assumes both reads to succeed, and in fact to return the correct value,
 at least one of them always, for the combining function to select that side.
`const` would always select the first, `flip const` the second.
But now there is no possible function to combine them again, because 
TODO loses `<|>`ing the possible parses


## Group theory ##

In theory, all `Format t` form a semiring, using the appropriate combinators:
```
instance Semiring (Format a) where
 zero = FEmpty
 plus = FAlt
 one = FConstext ""
 times = FFork
```
Note that the laws only apply in terms of the formatting semantics.
For example, `FFork`ing with a `FEmpty` does not yield an actual `FEmpty`,
 but nonetheless a `Format` that behaves exactly like one.
TODO



# Formed #

`Formed` is a typeclass providing canonical or default `Format`s:
```
class Formed t where
 form :: Format t
```

TODO
