This is just me playing around with a few different type systems.

I'm looking at STLC, System F and System Fw as my core type systems.

From there I plan to play around with options on a few different axes.

In the short term I want to:

- add pattern matching
    - including redundancy and incompleteness checking
    - use the patterns in the lambda term
    - add warnings for unused variables in patterns
- add records and variants
- add let / fix / letrec
- it would be nice to have some kind of let / letrec distinction on function declarations
    - the idea being that if you don't have letrec / fix in your language, and you don't have
      recursive function declarations, then you don't have recursion
- add recursive types
    - splitting this into data and codata could be interesting
    - generating fold and unfold functions for recursive data types could also be interesting
        - especially for languages that don't otherwise have recursion
- add type classes
    - I think I want at least multi-parameter type classes and functional dependencies
    - I think I know how to either inline all of the typeclass code, or to have it all work by dictionary passing
- add row types
    - in the short term I'm only after a subset of that functionality
- play around with a few different options for type and kind inference
- look at annotating everything with source locations and at how nice I can make type errors

In the longer term I want to:

- play around with proving various PLT theorems (progress, preservation, normalization) by using a similar set of techniques in Coq
    - I have already approximated some of this with QuickCheck over in `little-languages`
- play around with compilation to LLVM
- play around with affine types
- play around with session types

Who knows how much I'll get around to.  In either case, this will likely be folded back into `little-languages` at some point.
