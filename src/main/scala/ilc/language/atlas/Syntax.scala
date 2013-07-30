/**
 * A description of the calculus Atlas
 *
 * Agda original:
 * https://github.com/ps-mr/ilc/blob/9aafd0c2835ff027b57e44ed2930f4f57147e0de/agda/Syntax/Language/Atlas.agda
 */

package ilc
package language.atlas

import scala.language.implicitConversions

object Syntax extends feature.Functions {

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
    App(App(App(Zip(keyType, valType1, valType2, resultType),
      f), map1), map2)

  def fold(keyType: Type, valType: Type,
           f: Term, z: Term, m: Term): Term =
    App(App(App(Fold(keyType, valType), f), z), m)

  // swap argument order
  def flip(t: Term): Term =
    Abs("x", Abs("y", App(App(t, Var("y")), Var("x"))))

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
    Abs("x", Abs("y", pair(sType, tType, Var("x"), Var("y"))))

  def uncurry(type1: Type, type2: Type, f: Term, p: Term): Term = {
    val Map(t1, t2) = pairType(type1, type2)
    val Map(t3, t4) = t2
    val List(x, y, yt, dontcare) =
      uniqueNames(f, "x", "y", "yt", "_")
    App(App(App(Fold(t1, t2),
      Abs(x, Abs(yt, Abs(dontcare,
        App(App(App(Fold(t3, t4),
          Abs(y, Abs(dontcare, Abs(dontcare,
            App(App(f, Var(x)), Var(y)))))),
          App(App(f, Var(x)), neutralTerm(type2))),
          Var(yt)))))),
      App(App(f, neutralTerm(type1)), neutralTerm(type2))),
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
    // g = λ key p12 p34 → uncurry
    //       (λ val1 val2 → uncurry
    //         (λ val3 val4 →
    //           fw key val1 val2 val3 val4)
    //         p34)
    //       p12
    val List(key, p12, p34, val1, val2, val3, val4) =
      uniqueNames(f, "k", "p₁₂", "p₃₄", "v₁", "v₂", "v₃", "v₄").map(Var)
    val g =
      Abs(key.name, Abs(p12.name, Abs(p34.name, uncurry(v1, v2,
        Abs(val1.name, Abs(val2.name, uncurry(v3, v4,
          Abs(val3.name, Abs(val4.name, App(App(App(App(App
            (f, key), val1), val2), val3), val4))),
          p34))),
        p12))))
    zip(k, pairType(v1, v2), pairType(v3, v4), resultType,
      g,
      zipPair(k, v1, v2, pairType(v1, v2), m1, m2),
      zipPair(k, v3, v4, pairType(v3, v4), m3, m4))
  }

  def diffTerm(tau: Type): Term = tau match {
    // b₁ ⊝ b₀ = b₁ xor b₀
    case Bool => Xor
    // n₁ ⊝ n₀ = (n₀, n₁ - n₀) // old -> summand
    case Number => Abs("n₁", Abs("n₀",
        mapLit(Number, Number,
          Var("n₀") ->
            App(App(Plus, Var("n₁")), App(Negate, Var("n₀"))))))
    // m₁ ⊝ m₀ = zip _⊝_  m₁ m₀
    case Map(k, v) =>
      App(Zip(k, v, v, deltaType(v)), Abs("_", diffTerm(v)))
  }

  def applyTerm(tau: Type): Term = tau match {
    // apply Δb b = Δb xor b
    case Bool => Xor

    // apply Δn n = n + lookup n Δn
    // replace by new value... if old one is correct
    case Number => Abs("Δn", Abs("n",
      App(App(Plus, Var("n")),
        App(App(Lookup(Number, Number), Var("n")), Var("Δn")))))

    // apply Δm m = zip apply Δm m
    case Map(k, v) =>
      App(Zip(k, deltaType(v), v, v), Abs("_", applyTerm(v)))
  }

  def applyBaseFun(types: Type*): Term =
    if (types.length < 1)
      sys error "too few types"
    else if (types.length == 1)
      applyTerm(types.head)
    else
      Abs("Δf", Abs("f", Abs("x",
        App(App(applyBaseFun(types.tail: _*),
          App(App(Var("Δf"), Var("x")), nilTerm(types.head))),
          App(Var("f"), Var("x"))))))

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

    // Constants
    case True  => False
    case False => False
    case Num(n) => Empty(Number, Number)
    case Empty(k, v) => Empty(k, deltaType(v))

    // λx. λΔx. λy. λΔy. Xor Δx Δy
    case Xor => Abs("x", Abs("Δx", Abs("y", Abs("Δy",
                    App(App(Xor, Var("Δx")), Var("Δy"))))))

    // λx. λΔx. λy. λΔy. Map(x + y -> lookup x Δx + lookup y Δy)
    case Plus => Abs("x", Abs("Δx", Abs("y", Abs("Δy",
                   mapLit(Number, Number,
                          App(App(Plus, Var("x")), Var("y")) ->
                          App(App(Plus,
                            App(App(Lookup(Number, Number),
                              Var("x")), Var("Δx"))),
                            App(App(Lookup(Number, Number),
                              Var("y")), Var("Δy"))))))))

    // λx. λΔx. Map(x -> - lookup x Δx)
    case Negate => Abs("x", Abs("Δx", mapLit(Number, Number,
                     App(Negate, Var("x")) ->
                     App(Negate,
                       App(App(Lookup(Number, Number),
                         Var("x")), Var("Δx"))))))

    // λ k Δk v Δv m Δm →
    //   let
    //     k' = apply Δk k
    //   in
    //     update k' (diff (apply Δv v) (lookup k' (update k v m)))
    //       (update k (diff (apply Δm[k] m[k]), v) Δm)
    case Update(k, v) => {
      val update: Term = Update(k, deltaType(v))
      val List(key, dKey, value, dValue, map, dMap) =
        List("k", "Δk", "v", "Δv", "m", "Δm").map(Var)
      val newKey: Term = App(App(applyTerm(k), dKey  ), key  )
      val newVal: Term = App(App(applyTerm(v), dValue), value)
      Abs(key.name, Abs(dKey.name, Abs(value.name, Abs(dValue.name,
       Abs(map.name, Abs(dMap.name,
        App(App(App(update, newKey),
          App(App(diffTerm(v), newVal),
            App(App(Lookup(k, v), newKey),
              App(App(App(Update(k, v), key), value), map)))),
        App(App(App(update, key),
          App(App(diffTerm(v),
            App(App(applyTerm(v),
              App(App(Lookup(k, deltaType(v)), key), dMap)),
              App(App(Lookup(k, v), key), map))),
            value)),
          dMap))))))))
    }

    // λ k Δk m Δm →
    //   diff (apply (lookup (apply Δk k) Δm)
    //               (lookup (apply Δk k) m))
    //        (lookup k m)
    case Lookup(k, v) => {
      val List(key, dKey, map, dMap) =
        List("k", "Δk", "m", "Δm").map(Var)
      val newKey = App(App(applyTerm(k), dKey), key)
      Abs(key.name, Abs(dKey.name, Abs(map.name, Abs(dMap.name,
        App(App(diffTerm(v),
          App(App(applyTerm(v),
            App(App(Lookup(k, deltaType(v)), newKey), dMap)),
            App(App(Lookup(k, v), newKey), map))),
          App(App(Lookup(k, v), key), map))))))
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
      val List(f, df, m1, dm1, m2, dm2) =
        List("f", "Δf", "m₁", "Δm₁", "m₂", "Δm₂").map(Var)
      Abs(f.name, Abs(df.name, Abs(m1.name, Abs(dm1.name,
      Abs(m2.name, Abs(dm2.name,
        App(App(diffTerm(Map(k, w)),
          zip(k, v1, v2, w,
              App(App(applyBaseFun(k, v1, v2, w), df), f),
              App(App(applyTerm(Map(k, v1)), dm1), m1),
              App(App(applyTerm(Map(k, v2)), dm2), m2))),
          zip(k, v1, v2, w, f, m1, m2))))))))
    }
  }
}
