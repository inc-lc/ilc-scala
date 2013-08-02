/**
 * A description of the calculus Atlas
 *
 * Agda original:
 * https://github.com/ps-mr/ilc/blob/master/agda/Syntax/Language/Atlas.agda
 */

package ilc
package language.atlas

import scala.language.implicitConversions

trait Syntax extends feature.Functions {

  sealed trait Constant

  trait NestingBinaryOperator { self: Constant =>
    // easy way to build up nested addition
    def apply(lhs: Term, rhs: Term, others: Term*): Term =
      if (others.isEmpty)
        Const(this)(lhs)(rhs)
      else
        apply(apply(lhs, rhs), others.head, others.tail: _*)
  }

  // Diff and Apply are primitives that cannot be derived.
  // They are type-indexed terms in Agda, but here, without
  // types, they have to be primitives.
  case object Diff  extends Constant
  case object Apply extends Constant

  case object True  extends Constant
  case object False extends Constant
  case object Xor   extends Constant

  case class Num(n: Int) extends Constant
  case object Plus extends Constant with NestingBinaryOperator
  case object Negate extends Constant

  case object Empty  extends Constant
  case object Update extends Constant
  case object Lookup extends Constant
  case object Zip    extends Constant
  case object Fold   extends Constant

  // syntactic sugars for constants and terms via implict conversion
  implicit def intToConstant(i: Int): Constant = Num(i)
  implicit def intToTerm(i: Int): Term = Const(intToConstant(i))

  // easy construction of map literals
  def Map(assoc: (Term, Term)*): Term =
    updatesFrom(Empty, assoc: _*)

  def fromList(base: Term, assoc: List[(Term, Term)]): Term =
    assoc match {
      case Nil => base
      case (k, v) :: assoc =>
        Update(k)(v)(fromList(base, assoc))
    }

  // shorthand for chain updates
  def updatesFrom(base: Term, assoc: (Term, Term)*): Term =
    fromList(base, assoc.toList)

  // pairs encoded as maps
  // A × B = Map[A, Map[B, Bool]]
  // a , b = Map(a -> Map(b -> True))

  def pair(s: Term, t: Term): Term = Map(s -> Map(t -> True))

  val pairTerm: Term =
    Lambda("x", "y") ->: pair("x", "y")

  def uncurry(f: Term, p: Term): Term = {
    val List(x, y, yt, dontcare) =
      uniqueNames(f, "x", "y", "yt", "_")
    Fold(
      Lambda(x, yt, dontcare) ->:
        Fold(
          Lambda(y, dontcare, dontcare) ->: f(x)(y))(
          f(x)(Empty))(
          yt))(
      f(Empty)(Empty))(
      p)
      // you may wonder at the occurrences of Empty here
      // and may ask, why is the empty map given to the
      // function f as an argument, where f could well
      // be expecting integers?
      //
      // the answer is: we need a syntax term that
      // denotes the neutral element in the dynamic type
      // system. Right now, we have no dedicated symbol
      // for the neutral element; False, Num(0) and Empty
      // will all evaluate to Value.Neutral, and Empty
      // seems to be the least confusion of the lot.
  }

  def zipPair(map1: Term, map2: Term): Term =
    Zip("_" ->: pairTerm)(map1)(map2)

  def zip4(f: Term, // k → v1 → v2 → v3 → v4 → b
           m1: Term, m2: Term, m3: Term, m4: Term): Term = {
    // g = λ key p12 p34 →
    //       uncurry
    //         (λ val1 val2 →
    //           uncurry
    //             (λ val3 val4 → f key val1 val2 val3 val4)
    //             p34)
    //         p12
    val List(key, p12, p34, val1, val2, val3, val4) =
      uniqueVars(f, "k", "p₁₂", "p₃₄", "v₁", "v₂", "v₃", "v₄")
    val g = Lambda(key, p12, p34) ->:
      uncurry(
        Lambda(val1, val2) ->:
          uncurry(
            Lambda(val3, val4) ->: f(key)(val1)(val2)(val3)(val4),
            p34),
        p12)
    Zip(g)(zipPair(m1, m2))(zipPair(m3, m4))
  }

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
  }
}
