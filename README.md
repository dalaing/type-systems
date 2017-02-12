This is just me playing around with a few different type systems.

I'm looking at STLC, System F and System Fw as my core type systems.

From there I plan to play around with options on a few different axes.

In the short term I want to:

- add sum and product types
- add pattern matching
    - including redundancy and incompleteness checking
- add recursive types
- add type classes
    - I want to do this without dictionary passing
    - I think I want at least multi-parameter type classes and functional dependencies
- add row types
    - in the short term I'm only after a subset of that functionality
- play around with a few different options for type and kind inference
- look at annotating everything with source locations and at how nice I can make type errors

In the longer term I want to:

- play around with compilation to LLVM
- play around with affine types
- play around with session types

Who knows how much I'll get around to.  In either case, this will likely be folded back into `little-languages` at some point.
