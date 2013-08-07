package ilc
package language.atlas

/**
 * Symbolic derivation for Atlas
 */
trait Derivation extends feature.functions.Derivation { self: Syntax =>
  def deriveConst(c: Constant): Term = c match {
    // underivables
    case Diff | Apply => sys.error("cannot derive " ++ c.toString)

    // Constants
    case True  => False
    case False => False
    case Num(n) => Empty
    case Empty => Empty

    // λx. λΔx. λy. λΔy. Xor Δx Δy
    case Xor => Lambda("x", "Δx", "y", "Δy") ->: Xor("Δx")("Δy")

    // λx. λΔx. λy. λΔy. Map(x + y -> lookup x Δx + lookup y Δy)
    case Plus => Lambda("x", "Δx", "y", "Δy") ->:
      Map(Plus("x", "y") ->
          Plus(Lookup("x")("Δx"),
               Lookup("y")("Δy")))

    // λx. λΔx. Map(x -> - lookup x Δx)
    case Negate => Lambda("x", "Δx") ->:
      Map(Negate("x") ->
          Negate(Lookup("x")("Δx")))

    // λ k Δk v Δv m Δm →
    //   let
    //     k' = apply Δk k
    //   in
    //     update k' (diff (apply Δv v) (lookup k' (update k v m)))
    //       (update k (diff (apply Δm[k] m[k]) v) Δm)
    case Update => {
      val newKey: Term = Apply("Δk")("k")
      val newVal: Term = Apply("Δv")("v")
      Lambda("k", "Δk", "v", "Δv", "m", "Δm") ->:
        Update(newKey)(
          Diff(newVal)(
            Lookup(newKey)(Update("k")("v")("m"))))(
        Update("k")(
          Diff(
            Apply(
              Lookup("k")("Δm"))(
              Lookup("k")("m")))(
            "v"))(
          "Δm"))
    }

    // λ k Δk m Δm →
    //   diff (apply (lookup (apply Δk k) Δm)
    //               (lookup (apply Δk k) m))
    //        (lookup k m)
    case Lookup => {
      val newKey = Apply("Δk")("k")
      Lambda("k", "Δk", "m", "Δm") ->:
        Diff(
          Apply(
            Lookup(newKey)("Δm"))(
            Lookup(newKey)("m")))(
          Lookup("k")("m"))
    }

    // This would be nice, but it can't handle deletion from both
    // m1 and m2.
    //
    //   λ f Δf m₁ Δm₁ m₂ Δm₂ → zip4 Δf m₁ Δm₁ m₂ Δm₂
    //
    // Instead, the unoptimized derivative of Zip must recompute.
    //
    //   λ f Δf m₁ Δm₁ m₂ Δm₂ → diff
    //     (zip (apply Δf f) (apply Δm₁ m₁) (apply Δm₂ m₂))
    //     (zip f m₁ m₂)
    //
    // which is definitionally equal to
    //
    //   diff zip zip
    //
    case Zip => Diff(Zip)(Zip)

    // similarly, the derivative of Fold in
    // 9aafd0c2835ff027b57e44ed2930f4f57147e0de
    // can't handle deletions. we recompute again.
    case Fold => Diff(Fold)(Fold)
  }
}
