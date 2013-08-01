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

  trait NestingBinaryOperator { self: Constant =>
    // easy way to build up nested addition
    def apply(lhs: Term, rhs: Term, others: Term*): Term =
      if (others.isEmpty)
        Const(this)(lhs)(rhs)
      else
        apply(apply(lhs, rhs), others.head, others.tail: _*)
  }

  case object True  extends Constant
  case object False extends Constant
  case object Xor   extends Constant

  case class Num(n: Int) extends Constant
  case object Plus extends Constant with NestingBinaryOperator
  case object Negate extends Constant

  case class Empty(k: Type, v: Type)  extends Constant
  case class Update(k: Type, v: Type) extends Constant
  case class Lookup(k: Type, v: Type) extends Constant
  case class Zip(k: Type, u: Type, v: Type, w: Type) extends Constant
  case class Fold(k: Type, v: Type) extends Constant

  // syntactic sugars for constants and terms via implict conversion
  implicit def intToConstant(i: Int): Constant = Num(i)
  implicit def intToTerm(i: Int): Term = Const(intToConstant(i))
  implicit def liftPair[S, T]
    (p: (S, T))
    (implicit impS: S => Term, impT: T => Term): (Term, Term) =
      (impS(p._1), impT(p._2))

  // shorthand term constructors
  def zip(keyType: Type, valType1: Type, valType2: Type,
          resultType: Type,
          f: Term, map1: Term, map2: Term): Term =
    Zip(keyType, valType1, valType2, resultType)(f)(map1)(map2)

  def fold(keyType: Type, valType: Type,
           f: Term, z: Term, m: Term): Term =
    Fold(keyType, valType)(f)(z)(m)

  // swap argument order
  def flip(t: Term): Term =
    "x" ->: "y" ->: t("y")("x")

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
        Update(keyType, valType)(k)(v)(
          fromList(keyType, valType, base, assoc))
    }

  // shorthand for chain updates
  def updatesFrom(keyType: Type,
                  valType: Type,
                  base: Term,
                  assoc: (Term, Term)*): Term =
    fromList(keyType, valType, base, assoc.toList)

  // pairs encoded as maps
  // A × B = Map[A, Map[B, Bool]]
  // a , b = mapLit(a -> mapLit(b -> True))

  def pairType(type1: Type, type2: Type): Type =
    Map(type1, Map(type2, Bool))

  def pair(sType: Type, tType: Type, s: Term, t: Term): Term = {
    val Map(t1, t2) = pairType(sType, tType)
    val Map(t3, t4) = t2
    mapLit(t1, t2, s -> mapLit(t3, t4, t -> True))
  }

  def pairTerm(sType: Type, tType: Type): Term =
    Lambda("x", "y") ->: pair(sType, tType, "x", "y")

  def uncurry(type1: Type, type2: Type, f: Term, p: Term): Term = {
    val Map(t1, t2) = pairType(type1, type2)
    val Map(t3, t4) = t2
    val List(x, y, yt, dontcare) =
      uniqueNames(f, "x", "y", "yt", "_")
    Fold(t1, t2)(
      Lambda(x, yt, dontcare) ->:
        Fold(t3, t4)(
          Lambda(y, dontcare, dontcare) ->: f(x)(y))(
          f(x)(neutralTerm(type2)))(
          yt))(
      f(neutralTerm(type1))(neutralTerm(type2)))(
      p)
  }

  def zipPair(keyType: Type,
              valType1: Type,
              valType2: Type,
              resultType: Type,
              map1: Term,
              map2: Term): Term =
    zip(keyType, valType1, valType2, resultType,
        // pair-term needn't be weakened because it's closed
        // relevant test: "weakening closed terms has no effect"
        Abs("_", pairTerm(valType1, valType2)),
        map1, map2)

  def zip4(k: Type, v1: Type, v2: Type, v3: Type, v4: Type,
           resultType: Type,
           f: Term, // k → v1 → v2 → v3 → v4 → b
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
      uncurry(v1, v2,
        Lambda(val1, val2) ->:
          uncurry(v3, v4,
            Lambda(val3, val4) ->: f(key)(val1)(val2)(val3)(val4),
            p34),
        p12)
    zip(k, pairType(v1, v2), pairType(v3, v4), resultType,
      g,
      zipPair(k, v1, v2, pairType(v1, v2), m1, m2),
      zipPair(k, v3, v4, pairType(v3, v4), m3, m4))
  }

  def diffTerm(tau: Type): Term = tau match {
    // b₁ ⊝ b₀ = b₁ xor b₀
    case Bool => Xor
    // n₁ ⊝ n₀ = (n₀, n₁ - n₀) // old -> summand
    case Number => Lambda("n₁", "n₀") ->:
        mapLit(Number, Number, Var("n₀") -> Plus("n₁", Negate("n₀")))
    // m₁ ⊝ m₀ = zip _⊝_  m₁ m₀
    case Map(k, v) =>
      Zip(k, v, v, deltaType(v))("_" ->: diffTerm(v))
  }

  def applyTerm(tau: Type): Term = tau match {
    // apply Δb b = Δb xor b
    case Bool => Xor

    // apply Δn n = n + lookup n Δn
    // replace by new value... if old one is correct
    case Number => Lambda("Δn", "n") ->:
      Plus("n", Lookup(Number, Number)("n")("Δn"))

    // apply Δm m = zip apply Δm m
    case Map(k, v) =>
      Zip(k, deltaType(v), v, v)("_" ->: applyTerm(v))
  }

  def applyBaseFun(types: Type*): Term =
    if (types.length < 1)
      sys error "too few types"
    else if (types.length == 1)
      applyTerm(types.head)
    else
      Lambda("Δf", "f", "x") ->:
        applyBaseFun(types.tail: _*)(
          Var("Δf")("x")(nilTerm(types.head)))(
          Var("f")("x"))

  def neutralTerm(tau: Type): Term = tau match {
    case Bool => False
    case Number => 0
    case Map(k, v) => Empty(k, v)
  }

  def nilTerm(tau: Type): Term = neutralTerm(deltaType(tau))

  def insert(keyType: Type,
             valType: Type,
             key: Term,
             value: Term,
             deltaMap: Term): Term = {
    Update(keyType, deltaType(valType))(
      key)(
      diffTerm(valType)(value)(neutralTerm(valType)))(
      deltaMap)
  }

  def delete(keyType: Type,
             valType: Type,
             key: Term,
             value: Term,
             deltaMap: Term): Term = {
    Update(keyType, deltaType(valType))(key)(
      diffTerm(valType)(neutralTerm(valType))(value))(
      deltaMap)
  }

  def deriveConst(c: Constant): Term = c match {

    // Constants
    case True  => False
    case False => False
    case Num(n) => Empty(Number, Number)
    case Empty(k, v) => Empty(k, deltaType(v))

    // λx. λΔx. λy. λΔy. Xor Δx Δy
    case Xor => Lambda("x", "Δx", "y", "Δy") ->: Xor("Δx")("Δy")

    // λx. λΔx. λy. λΔy. Map(x + y -> lookup x Δx + lookup y Δy)
    case Plus => Lambda("x", "Δx", "y", "Δy") ->:
      mapLit(Number, Number,
        Plus("x", "y") ->
          Plus(Lookup(Number, Number)("x")("Δx"),
               Lookup(Number, Number)("y")("Δy")))

    // λx. λΔx. Map(x -> - lookup x Δx)
    case Negate => Lambda("x", "Δx") ->:
      mapLit(Number, Number,
        Negate("x") ->
          Negate(Lookup(Number, Number)("x")("Δx")))

    // λ k Δk v Δv m Δm →
    //   let
    //     k' = apply Δk k
    //   in
    //     update k' (diff (apply Δv v) (lookup k' (update k v m)))
    //       (update k (diff (apply Δm[k] m[k]) v) Δm)
    case Update(k, v) => {
      val update: Term = Update(k, deltaType(v))
      val newKey: Term = applyTerm(k)("Δk")("k")
      val newVal: Term = applyTerm(v)("Δv")("v")
      Lambda("k", "Δk", "v", "Δv", "m", "Δm") ->:
        update(newKey)(
          diffTerm(v)(newVal)(
            Lookup(k, v)(newKey)(Update(k, v)("k")("v")("m"))))(
        update("k")(
          diffTerm(v)(
            applyTerm(v)(
              Lookup(k, deltaType(v))("k")("Δm"))(
              Lookup(k, v)("k")("m")))(
            "v"))(
          "Δm"))
    }

    // λ k Δk m Δm →
    //   diff (apply (lookup (apply Δk k) Δm)
    //               (lookup (apply Δk k) m))
    //        (lookup k m)
    case Lookup(k, v) => {
      val newKey = applyTerm(k)("Δk")("k")
      Lambda("k", "Δk", "m", "Δm") ->:
        diffTerm(v)(
          applyTerm(v)(
            Lookup(k, deltaType(v))(newKey)("Δm"))(
            Lookup(k, v)(newKey)("m")))(
          Lookup(k, v)("k")("m"))
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
    case Zip(k, v1, v2, w) => {
      Lambda("f", "Δf", "m₁", "Δm₁", "m₂", "Δm₂") ->:
        diffTerm(Map(k, w))(
          zip(k, v1, v2, w,
              applyBaseFun(k, v1, v2, w)("Δf")("f"),
              applyTerm(Map(k, v1))("Δm₁")("m₁"),
              applyTerm(Map(k, v2))("Δm₂")("m₂")))(
          zip(k, v1, v2, w, "f", "m₁", "m₂"))
    }
  }
}
