/**
 * A description of the calculus Atlas
 *
 * Agda original:
 * https://github.com/ps-mr/ilc/blob/master/agda/Syntax/Language/Atlas.agda
 */

package ilc
package language.atlas

import scala.language.implicitConversions

trait Syntax
extends feature.Functions
   with feature.DiffAndApply {

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
  val diffTerm = Const(Diff)
  val applyTerm = Const(Apply)

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

  // neutralTerm/nilTerm denotes the neutral element/nil change
  // of all base-type terms
  val neutralTerm = Empty
  val nilTerm = Empty

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
          f(x)(neutralTerm))(
          yt))(
      f(neutralTerm)(neutralTerm))(
      p)
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
}
