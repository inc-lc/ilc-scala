/**
 * A description of the calculus Atlas
 *
 * Agda original:
 * https://github.com/ps-mr/ilc/blob/9aafd0c2835ff027b57e44ed2930f4f57147e0de/agda/Syntax/Language/Atlas.agda
 */

package Language

import scala.language.implicitConversions

object Atlas extends Syntax.Lambda {

  sealed trait Type

  case object Bool extends Type
  case object Number extends Type
  case class Map(k: Type, v: Type) extends Type

  def deltaType(iota: Type): Type = iota match {
    case Bool => Bool
    case Number => Map(Number, Number) // old -> summand
    case Map(k, v) => Map(k, deltaType(v))
  }

  sealed trait Constant

  case object True  extends Constant
  case object False extends Constant
  case object Xor   extends Constant

  case class Num(n: Int) extends Constant {
    override def toString = n.toString
  }
  case object Plus extends Constant

  case class Empty(k: Type, v: Type)  extends Constant
  case class Update(k: Type, v: Type) extends Constant
  case class Lookup(k: Type, v: Type) extends Constant
  case class Zip(k: Type, u: Type, v: Type) extends Constant
  case class Fold(k: Type, v: Type, b: Type) extends Constant

  // syntactic sugars for constants and terms via implict conversion
  implicit def intToConstant(i: Int): Constant = Num(i)
  implicit def intToTerm(i: Int): Term = Const(intToConstant(i))
  implicit def liftPair[S, T]
    (p: (S, T))
    (implicit impS: S => Term, impT: T => Term): (Term, Term) =
      (impS(p._1), impT(p._2))

  // shorthand term constructors
  def zip(keyType: Type, valType1: Type, valType2: Type,
          f: Term, map1: Term, map2: Term): Term =
    App(App(App(Zip(keyType, valType1, valType2), f), map1), map2)

  def fold(keyType: Type, valType: Type, resultType: Type,
           f: Term, z: Term, m: Term): Term =
    App(App(App(Fold(keyType, valType, resultType), f), z), m)

  // swap argument order
  def flip(t: Term): Term =
    Abs("x", Abs("y", App(App(t, Var(0)), Var(1))))

  // easy construction of map literals
  def mapLit(keyType: Type,
             valType: Type,
             assoc: (Term, Term)*): Term =
    updatesFrom(keyType, valType, Empty(keyType, valType), assoc: _*)

  def fromList(keyType: Type,
               valType: Type,
               base: Term,
               assoc: List[(Term, Term)]): Term =
    assoc match {
      case Nil => base
      case (k, v) :: assoc =>
        App(App(App(Update(keyType, valType), k), v),
                    fromList(keyType, valType, base, assoc))
    }

  // shorthand for chain updates
  def updatesFrom(keyType: Type,
                  valType: Type,
                  base: Term,
                  assoc: (Term, Term)*): Term =
    fromList(keyType, valType, base, assoc.toList)

  // pairs encoded as maps
  def pair(sType: Type, tType: Type, s: Term, t: Term): Term = {
    val update: Term = Update(sType, tType)
    val empty : Term = Empty (sType, tType)
    App(App(update, s), App(App(update, t), empty))
  }

  def pairTerm(sType: Type, tType: Type): Term =
    Abs("x", Abs("y", pair(sType, tType, Var(1), Var(0))))

  def diffTerm(tau: Type): Term = tau match {
    // b₀ ⊝ b₁ = b₀ xor b₁
    case Bool => Xor
    // n₀ ⊝ n₁ = (n₀, n₁) // replacement pair
    case Number => pairTerm(Number, Number)
    // m₀ ⊝ m₁ = zip _⊝_  m₀ m₁
    case Map(k, v) => App(Zip(k, v, v), Abs("_", diffTerm(v)))
  }

  def applyTerm(tau: Type): Term = tau match {
    // b ⊕ Δb = b xor Δb
    case Bool => Xor

    // n ⊕ Δn = lookup n Δn
    // replace by new value... if old one is correct
    case Number => flip(Lookup(Number, Number))

    // m ⊕ Δm = zip _⊕_ m Δm
    case Map(k, v) =>
      App(Zip(k, v, deltaType(v)), Abs("_", applyTerm(v)))
  }

  def neutralTerm(tau: Type): Term = tau match {
    case Bool => False
    case Number => 0
    case Map(k, v) => Empty(k, v)
  }

  def insert(keyType: Type,
             valType: Type,
             key: Term,
             value: Term,
             deltaMap: Term): Term = {
    App(App(App(Update(keyType, deltaType(valType)), key),
      App(App(diffTerm(valType), value), neutralTerm(valType))),
      deltaMap)
  }

  def delete(keyType: Type,
             valType: Type,
             key: Term,
             value: Term,
             deltaMap: Term): Term = {
    App(App(App(Update(keyType, deltaType(valType)), key),
      App(App(diffTerm(valType), neutralTerm(valType)), value)),
      deltaMap)
  }

  def deriveConst(c: Constant): Term = c match {
    case True  => False
    case False => False

    // λx. λΔx. λy. λΔy. Xor Δx x
    case Xor => Abs("x", Abs("Δx", Abs("y", Abs("Δy",
                    App(App(Xor, Var(2)), Var(3))))))

    case Empty(k, v) => Empty(k, deltaType(v))
  }
}
