
***********************
Categorical Programming
***********************

DRAFT

`Categorical <https://en.wikipedia.org/wiki/Category_theory>`__  

In Manfred von Thun's article `Joy compared with other functional languages <http://www.kevinalbrecht.com/code/joy-mirror/j08cnt.html>`__ he asks, "Could the language of categories be used for writing programs? Any lambda expression can be translated into a categorical expression, so the language of categories is expressively complete. But this does not make it a suitable language for writing programs. As it stands it is a very low-level language."

In `Compiling to categories <http://conal.net/papers/compiling-to-categories/>`__ Conal Elliott give a taste of what this might mean.

    It is well-known that the simply typed lambda-calculus is modeled by any cartesian closed category (CCC). This correspondence suggests giving typed functional programs a variety of interpretations, each corresponding to a different category. A convenient way to realize this idea is as a collection of meaning-preserving transformations added to an existing compiler, such as GHC for Haskell. This paper describes such an implementation and demonstrates its use for a variety of interpretations including hardware circuits, automatic differentiation, incremental computation, and interval analysis. Each such interpretation is a category easily defined in Haskell (outside of the compiler). The general technique appears to provide a compelling alternative to deeply embedded domain-specific languages.

What he's doing is translating lambda forms into a kind of "point-free" style that is very close to Joy code (although more verbose) and then showing how to instantiate that code over different categories to get several different kinds of program out of the same code.

