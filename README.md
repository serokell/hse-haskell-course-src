### (Approximate) Syllabus

1. `stack`, `cabal`, and `Hello World` in Haskell. Module system. `GHC` and `GHCi`. `Hackage` and `Hoogle`.
2. Introduction to Haskell: Basic syntax, partial application, currying, infix operators. Lists, infinite lists, list comprehension.
3. Polymorphism: parametric polymorphism and ad-hoc polymorphism. Typeclasses and constraints. Classes `Eq`, `Ord`, `Num`, `Enum` and their instances.
4. Datatypes: data, newtype, type. Pattern-matching.
5. `Functor` and its laws, `Foldable`, `Monoid`. Examples.
6. `Applicative` and its laws, `Traversable`. Examples.
7. `Monad` and its laws, examples. Do-notation. Alternative and `MonadPlus`.
8. Monads: `IO`, `State`, `Writer`, `Reader`.
9. Monad transformers.
10. Concurrent and parallel Haskell: `async` library.

### Sources

#### Articles and blog posts

1. [Philip Wadler. Comprehending Monads](https://ncatlab.org/nlab/files/WadlerMonads.pdf)
2. [Philip Wadler. Monads for functional programming](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
3. [Philip Wadler. Theorems for Free](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)
4. [Simon Peyton Jones, Andrew Gordon, Sigbjorn Finne. Concurrent Haskell](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.47.7494&rep=rep1&type=pdf)
5. [Paul Hudak, John Hughes, Simon Peyton Jones, Philip Wadler. A history of Haskell: being lazy with class](https://dl.acm.org/doi/abs/10.1145/1238844.1238856)
6. [Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields. Practical type inference for arbitrary-rank types](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/abs/practical-type-inference-for-arbitraryrank-types/5339FB9DAB968768874D4C20FA6F8CB6)
7. [Cordelia V Hall, Kevin Hammond, Simon Peyton Jones, Philip L Wadler. Type classes in Haskell](https://dl.acm.org/doi/abs/10.1145/227699.227700)
8. [Heitor Toledo Lassarote de Paula. A Brief Introduction to Template Haskell](https://serokell.io/blog/introduction-to-template-haskell)
9. [Vladislav Zavialov. Type Families in Haskell: The Definitive Guide](https://serokell.io/blog/type-families-haskell)
10. [Vladislav Zavialov. Why Dependent Haskell is the Future of Software Development](https://serokell.io/blog/why-dependent-haskell)
11. [Vladislav Zavialov. Haskell to Core: Understanding Haskell Features Through Their Desugaring](https://serokell.io/blog/haskell-to-core)
12. [Simon Marlow, Simon Peyton Jones. The Glasgow Haskell Compiler](https://www.aosabook.org/en/ghc.html)

#### Books

1. [Miran Lipovača. Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
2. [Sandy Maguire. Thinking with Types: Type-Level Programming in Haskell](https://leanpub.com/thinking-with-types)
3. [Vitaly Bragilevsky. Haskell in Depth](https://www.manning.com/books/haskell-in-depth)
4. [Alejandro Serrano Mena. Practical Haskell](https://www.apress.com/gp/book/9781484244791)
5. [Graham Hutton. Programming in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html)

#### Types and lambda

1. [Benjamin C. Pierce. Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)
2. [Morten Heine Sørensen, Pawel Urzyczyn. Lectures on the Curry-Howard Isomorphism](https://www.elsevier.com/books/lectures-on-the-curry-howard-isomorphism/sorensen/978-0-444-52077-7)
3. [J. Roger Hindley. Basic Simple Type Theory](https://www.cambridge.org/core/books/basic-simple-type-theory/0C0B4DB68D19B96E2F2F8AAF8DD738C7)
4. [Henk Barendregt. Lambda calculi with types](https://home.ttic.edu/~dreyer/course/papers/barendregt.pdf)
5. [Therry Coquand. Type theory](https://plato.stanford.edu/entries/type-theory/)
6. [Jean-Yves Girard, Yves Lafont, Paul Taylor. Proofs and types](https://www.paultaylor.eu/stable/prot.pdf)

#### Videos

1. [Simon Peyton Jones. Haskell is Useless](https://www.youtube.com/watch?v=iSmkqocn0oQ)
2. [Simon Peyton Jones. A History of Haskell: being lazy with class](https://www.youtube.com/watch?v=06x8Wf2r2Mc)
3. [Philip Wadler. Propositions as types](https://www.youtube.com/watch?v=IOiZatlZtGU)
4. [Philip Wadler. The First Monad Tutorial](https://www.youtube.com/watch?v=yjmKMhJOJos)
5. [A Haskell course by Bartosz Milewski](https://www.youtube.com/playlist?list=PL0pwx9zqJ9IamHxRXTf34dC3JeQ2oYmfJ)
6. [A more comprehensive (than ours) Haskell course by Denis Moskvin, Computer Science Centre (in Russian)](https://compscicenter.ru/courses/func-prog/2019-spring/)
7. [A lecture course on GHC by Vitaly Bragilevsky, Computer Science Club (in Russian)](https://www.lektorium.tv/node/32421)
8. [A lecture course on type inference by Vitaly Bragilevsky, Computer Science Club (in Russian)](https://compsciclub.ru/courses/2019-spring/6.423-types/)
9. [Vladislav Zavialov. Introduction to GUI programming in Haskell](https://www.youtube.com/watch?v=k1aq8ikO-8Q)
10. [Simon Marlow. Parallel and concurrent programming in Haskell](https://www.youtube.com/watch?v=lqG3mURwUxo)

### Guidelines

We strictly recommend you to follow the Haskell style guideline described in the [following link](https://github.com/serokell/style/blob/master/haskell.md) in order to make in your homework code more readable.
