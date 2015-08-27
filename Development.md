# Naming conventions

(To extend).

## Currently-used (and preferred) identifiers

### Variable names

Terms: t, term
Type: (t), typ, tau

# Architecture

Language features (as in sets of constructors) are represented by subpackages of
`ilc.feature` (ideally, orthogonal ones). In each such subpackage, separate traits define the:

- `Types` (optional)
- abstract `Syntax`, and optionally `SyntaxSugar`
- how to compile the syntax to Scala (`ToScala`)
- how to interpret the syntax (in `Evaluation`) (optional)
- how to derive programs using that syntax

However, the structure is not completely regular:
- as mentioned, some traits might be missing
- of some traits, there might be variants (like `AbelianDerivation`, that
  coexists in some features with `Derivation`).
- features are not always orthogonal
  - `Derivation` often requires importing other traits
  - many metaprograms are defined directly for a language including `functions`
    or `let`, sometimes even when one could conceivably modularize that, too.
    However, it's not clear that treating those as a separate feature was a good
    idea. There is a modularization overhead, especially for those constructors,
    and it's an ongoing pain.
- the amount of code required for linking is sometimes unfortunate. Moreover, it
  is not boring boilerplate, but tricky one, because one has to remember, for
  each operations, to mix in all traits implementing it for all language
  features supported.

  (It would be cool to have a support for open datatypes with coverage checking,
  as discussed with Tillmann, that supported this scenario. An extension
  mentions the base module it completes, and must restore completeness for that
  module; this would even allow for some support for nested pattern matching).

## Architectural erosion

`feature.let` includes also metaprogramming traits, which would belong in
`ilc.analysis` or more generically in `ilc.metaprogs`.

`ilc.analysis.FreeVariables` is extended by `ilc.let.FreeVariables`. The
pragmatic question there is: when we solve the expression problem, and add new
operations and new constructors, where do we place the code handling the new
operation for the new constructors?

`let.Context` is missing altogether.

Many modules have or need let-specific extensions. The source code is
inconsistent on whether `let` is part of the base language or not. In fact,
there are arguments for treating `let` as a key construct in intermediate
representations, and treating first-class functions as optional — this would
matter if we implemented defunctionalization.

## Problematic features

Pretty-printing is currently problematic, there's a ongoing refactoring on a
branch but it is on hold.

Type inference is also problematic. We first hacked together some ad-hoc form of
type inference, which is supported in the core code. The specification resembles
bidirectional type checking but (AFAIK) is more fragile, or less clearly
specified.

Next, we added support for Hindley-Milner type inference, but I'm reluctant to
only have that, in case we ever want to add language features that don't support
Hindley-Milner well (like subtyping).
