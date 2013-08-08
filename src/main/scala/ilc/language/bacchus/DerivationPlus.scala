package ilc
package language.bacchus

/**
 * Optimized derivation
 *
 * The machinery of optimized derivation resides in the
 * specialized form with Bacchus for the moment. We should
 * spend some thought unifying "derive" and "derivePlus"
 * under an identical interface so that the derivation
 * testing tool doesn't need to take the concrete trans-
 * formation as an additional parameter. Using
 *
 *   ilc.feature.functions.Subterm.children
 *
 * to discover constants (childless terms), we will be able
 * to make the "derive-everything-by-Diff" strategy a default
 * of all calculi.
 */

import ilc.feature.functions.Stability

trait DerivationPlus
extends language.bacchus.Derivation
   with Stability { self: Syntax =>
  def derivePlus(t: Term): Term = {
    val (stable, stableArg) = Stability_attr(t).split
    val FV = FV_attr(t)

    def recurse(s: Subterm): Term = s match {

      case Subterm.app(operator, operand) =>
        recurse(operator)(operand.term)(recurse(operand))

      case Subterm.abs(x, body) => {
        val dx = delta(x)
        if (FV_attr(t) contains dx)
          sys.error("naming scheme violation " ++
            "when deriving:\n  " ++ t.toString)
        else
          Lambda(x, dx) ->: recurse(body)
      }

      case Subterm.variable(x) => delta(x)

      // ΔUnit = Nothing ⊎ Unit
      case Subterm.const(Individualist) =>
        Right(Individualist)

      // changes to natural numbers are replacement pairs,
      // put in a sum so that the replacement-pair-part of
      // every base-type change is an injection with Right.
      //
      // ΔNat = Nothing ⊎ Nat
      //                  replace
      case Subterm.const(Nat(n)) =>
        Right(n)

      case Subterm.const(Plus) => {
        val irrelevant: Term = Lambda("_", "_") ->: Individualist
        Lambda("x", "Δx", "y", "Δy") ->:
          case4("Δx", "Δy",
            irrelevant, irrelevant, irrelevant,
            Lambda("xNew", "yNew") ->: Right(Plus("xNew", "yNew")))
      }

      // foldNat : r → (r → r) → Nat → r
      // the case where the number of iterations stay constant
      //
      // We call Δf and f n-times each. This is a situation
      // where caching intermediate results are helpful.
      //
      // The computation proceeds thus:
      // x₀ = z		Δx₀ = Δz
      // x₁ = f x₀	Δx₁ = Δf x₀ Δx₀
      // x₂ = f x₁	Δx₂ = Δf x₁ Δx₁
      // ...
      // until we manage to compute the change after the nth
      // iteration.
      //
      case Subterm.const(FoldNat) if stableArg(s, 2) => {
        val proj2: Term =
          "duo" ->: uncurry(Lambda("x", "y") ->: "y", "duo")
        Lambda("z", "Δz", "f", "Δf", "n", "Δn") ->:
          proj2(FoldNat(
            pair("z", "Δz"))(
            "duo" ->: uncurry(
              Lambda("x", "Δx") ->:
                pair(Var("f")("x"), Var("Δf")("x")("Δx")),
              "duo"))(
            "n"))
      }

      // Δ (Map κ τ) = Map κ ((Unit ⊎ τ) ⊎ Δτ) ⊎ Map κ τ
      //                      del  ins  modify  replace
      case Subterm.const(Empty) =>
        Left(Empty)

      // Δ (σ ⊎ τ) = (Δσ ⊎ Δτ) ⊎ (σ ⊎ τ)
      //              modify     replace
      case Subterm.const(Left) =>
        Lambda("x", "Δx") ->: Left(Left("Δx"))
      case Subterm.const(Right) =>
        Lambda("x", "Δx") ->: Left(Right("Δx"))

      // for constants we don't care about
      case Subterm.const(c) => deriveConst(c)
    }

    recurse(Subterm.refl(t))
  }
}
