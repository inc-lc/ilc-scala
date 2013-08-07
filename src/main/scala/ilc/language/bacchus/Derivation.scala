package ilc
package language.bacchus

/**
 * Symbolic derivation for Bacchus
 */
trait Derivation extends feature.functions.Derivation { self: Syntax =>
  def deriveConst(c: Constant): Term = c match {
    case Diff | Apply => sys.error("cannot derive " ++ c.toString)

    // ΔUnit = Nothing ⊎ Unit
    case Individualist => Right(Individualist)

    // changes to natural numbers are replacement pairs,
    // put in a sum so that the replacement-pair-part of
    // every base-type change is an injection with Right.
    //
    // ΔNat = Nothing ⊎ Nat
    //                  replace
    case Nat(n) => Right(n)

    case FoldNat => Diff(FoldNat)(FoldNat)

    case Plus => Lambda("x", "Δx", "y", "Δy") ->: {
      val irrelevant: Term = Lambda("_", "_") ->: Individualist
      case4("Δx", "Δy",
        irrelevant, irrelevant, irrelevant,
        Lambda("xNew", "yNew") ->: Right(Plus("xNew", "yNew")))
    }

    // Δ (Map κ τ) = Map κ ((Unit ⊎ τ) ⊎ Δτ) ⊎ (Map κ τ × Map κ τ)
    //                      del  ins  modify     replace
    case Empty => Left(Empty)

    // recompute everything for now.
    // consider potential improvements even here.
    // https://github.com/ps-mr/ilc/commit/053a8229f200d6087fe331c6ea7bccc2ba92b81d#commitcomment-3781686

    case Update => Diff(Update)(Update)
    case Lookup => Diff(Lookup)(Lookup)
    case Fold => Diff(Fold)(Fold)

    // Δ (σ ⊎ τ) = (Δσ ⊎ Δτ) ⊎ ((σ ⊎ τ) ⊎ (σ ⊎ τ))
    //              modify      replace
    case Left => Lambda("x", "Δx") ->: Left(Left("Δx"))
    case Right => Lambda("x", "Δx") ->: Left(Right("Δx"))
    case Either => Diff(Either)(Either)
  }
}
