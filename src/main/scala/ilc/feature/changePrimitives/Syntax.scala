package ilc
package feature
package changePrimitives

trait Syntax extends base.Syntax with feature.DiffAndApply {
  // Diff and Apply are primitives that cannot be derived.
  // They are type-indexed terms in Agda, but here, without
  // types, they have to be primitives.

  // From bacchus.Syntax:
  // type-indexed terms that has to be simulated in scala
  // cannot be derived
  //
  // diff : τ → τ → Δτ
  // apply : Δτ → τ → τ
  //
  case object Diff  extends Constant
  case object Apply extends Constant

  override val diffTerm = Const(Diff)
  override val applyTerm = Const(Apply)
}
