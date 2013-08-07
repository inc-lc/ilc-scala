package ilc
package language.bacchus

/**
 * Bacchus's evaluator
 */

import scala.language.implicitConversions
import scala.collection.immutable
import ilc.feature.functions

trait Evaluation
extends functions.Evaluation { self: language.bacchus.Syntax =>

  type ValueSum = Either[Value, Value]
  type ValueMap = immutable.Map[Value, Value]
  def ValueMap(assoc: (Value, Value)*): ValueMap =
    immutable.Map.apply[Value, Value](assoc: _*)

  // boilerplate for adding methods to Value trait
  implicit def toBacchus(value: Value): BacchusValue = value match {
    case bv: BacchusValue => bv
    case _ => new BacchusValue
  }

  class BacchusValue extends Value {
    def toNat: Int = die("toNat")
    def toMap: ValueMap = die("toMap")
    def toSum: ValueSum = die("toSum")
  }

  implicit def liftNat(n: Int): Value = Value.Nat(n)
  implicit def liftMap(m: ValueMap): Value = Value.Map(m)

  // boilerplate for extending value declarations
  override val Value = AtlasValueDeclarations
  object AtlasValueDeclarations extends ValueDeclarations {

    // the inhabitant of unit type has no computation content
    case object Individualist extends BacchusValue

    case class Nat(override val toNat: Int) extends BacchusValue {
      require(toNat >= 0)
    }

    case class Map(override val toMap: ValueMap) extends BacchusValue

    object Map {
      def apply(assoc: (Value, Value)*): Map =
        Map(immutable.Map(assoc: _*))
    }

    case class Sum(override val toSum: ValueSum) extends BacchusValue

    object Left {
      def apply(v: Value): Sum = Sum(scala.Left(v))
      def unapply(s: Sum): Option[Value] = s.toSum match {
        case scala.Left(v) => Some(v)
        case _ => None
      }
    }

    object Right {
      def apply(v: Value): Sum = Sum(scala.Right(v))
      def unapply(s: Sum): Option[Value] = s.toSum match {
        case scala.Right(v) => Some(v)
        case _ => None
      }
    }

    // helper to match against replacement pairs
    object Pair {
      def unapply(p: Map): Option[(Value, Value)] = {
        val encodedPair = p.toMap
        if (encodedPair.size == 1)
          Some(encodedPair.head)
        else
          None
      }
    }

    // helpers to match against map operations

    object Delete {
      def unapply(keyVal: (Value, Value)): Option[Value] =
        keyVal._2 match {
          case Left(Left(Individualist)) => Some(keyVal._1)
          case _ => None
        }
    }

    object Insert {
      def unapply(keyVal: (Value, Value)): Option[(Value, Value)] =
        keyVal._2 match {
          case Left(Right(newVal)) => Some(keyVal._1 -> newVal)
          case _ => None
        }
    }

    object Modify {
      def unapply(keyVal: (Value, Value)): Option[(Value, Value)] =
        keyVal._2 match {
          case Right(deltaVal) => Some(keyVal._1 -> deltaVal)
          case _ => None
        }
    }

    // helpers to identify keyset of deletion, insertion, modification

    def deletedKeys(dm: ValueMap): Set[Value] = dm.filter(_ match {
      case Delete(_) => true
      case _ => false
    }).keySet

    def insertedKeys(dm: ValueMap): Set[Value] = dm.filter(_ match {
      case Insert(_, _) => true
      case _ => false
    }).keySet

    def modifiedKeys(dm: ValueMap): Set[Value] = dm.filter(_ match {
      case Modify(_, _) => true
      case _ => false
    }).keySet

    def diff(u: Value, v: Value): Value = (u, v) match {
      case (Nat(n1), Nat(n2)) =>
        Right(Map(n1 -> n2))

      case (Map(m1), Map(m2)) =>
        Right(Map(m1 -> m2))

      case (Function(f), Function(g)) =>
        (x: Value) => (dx: Value) => diff(f(apply(dx, x)), g(x))
    }

    def apply(dv: Value, v: Value): Value = (v, dv) match {

      // ΔNat = Unit ⊎ (Nat × Nat)
      //         nil    replace
      case (Nat(n), Left(Individualist)) => n
      case (Nat(n), Right(Pair(nOld: Nat, nNew: Nat))) => {
        require(n == nOld)
        nNew
      }

      // Δ (Map κ τ) = Map κ ((Unit ⊎ τ) ⊎ Δτ) ⊎ (Map κ τ × Map κ τ)
      //                      del  ins  modify     replace
      case (Map(m), Left(Map(dm))) => {
        // sanity check:
        // 0. keys for deletion, insertion and modification
        //    are disjoint (guaranteed by map data structure)
        // 1. keys to be deleted exist
        // 2. keys to be inserted do not exist
        // 3. keys to be modified exist
        require((deletedKeys(dm) -- m.keySet).isEmpty)
        require((m.keySet -- insertedKeys(dm)).size == m.size)
        require((modifiedKeys(dm) -- m.keySet).isEmpty)

        dm.foldRight(m) { (keyVal, map) =>
          keyVal match {
            case Delete(key) => map - key

            case Insert(key, newVal) => map.updated(key, newVal)

            case Modify(key, dv) => map.updated(key, apply(dv, m(key)))
          }
        }
      }
      case (Map(m), Right(Pair(mOld: Map, mNew: Map))) => {
        require(m == mOld)
        mNew
      }

      // Δ (σ ⊎ τ) = (Δσ ⊎ Δτ) ⊎ ((σ ⊎ τ) ⊎ (σ ⊎ τ))
      //              modify      replace
      case (Left(v), Left(Left(dv))) =>
        Left(apply(dv, v))
      case (Right(v), Left(Right(dv))) =>
        Right(apply(dv, v))
      case (s: Sum, Pair(sOld: Sum, sNew: Sum)) => {
        require(s == sOld)
        sNew
      }

      case (Function(df), Function(f)) =>
        (x: Value) => apply(df(x)(diff(x, x)),  f(x))
    }
  }

  def evalConst(c: Constant): Value = c match {
    case Diff =>
      (u: Value) => (v: Value) => Value.diff(u, v)

    case Apply =>
      (dv: Value) => (v: Value) => Value.apply(dv, v)

    case Individualist =>
      Value.Individualist

    case Nat(n) =>
      n

    case FoldNat =>
      (z: Value) => (f: Value) => (n: Value) => {
        def loop(i: Int): Value =
          if (i == 0)
            z
          else
            f(loop(i - 1))
        loop(n.toNat)
      }

    case Plus =>
      (x: Value) => (y: Value) => x.toNat + y.toNat

    case Empty =>
      ValueMap()

    case Update =>
      (k: Value) => (v: Value) => (m: Value) =>
        m.toMap.updated(k, v)

    case Lookup =>
      (k: Value) => (m: Value) => m.toMap(k)

    case Fold =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))

    case Left =>
      (x: Value) => Value.Left(x)

    case Right =>
      (y: Value) => Value.Right(y)

    case Either =>
      (f: Value) => (g: Value) => (s: Value) =>
        s.toSum.fold[Value](x => f(x), y => g(y))
  }
}