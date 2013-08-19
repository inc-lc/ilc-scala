package ilc
package language.bacchus

/**
 * Bacchus's evaluator
 */

import scala.language.implicitConversions
import scala.collection.immutable
import ilc.feature._

trait Evaluation
extends functions.Evaluation with naturals.Evaluation with sum.Evaluation with maps.Evaluation with unit.Evaluation { self: language.bacchus.Syntax =>
  // boilerplate for extending value declarations
  override val Value = BacchusValueDeclarations

  // The internal structure of Map values is defined in terms of other language features, so it's separate from MapValues.
  trait MapValuesEncoding {
    this: MapValues with SumValues with UnitValues =>

    // helpers to match against map operations

    object Delete {
      def unapply(keyVal: (Value, Value)): Option[Value] =
        keyVal._2 match {
          case Left(Left(UnitValue)) => Some(keyVal._1)
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
  }

  object BacchusValueDeclarations extends FunValues with UnitValues with NatValues with SumValues with MapValues with MapValuesEncoding {
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

    def diff(u: Value, v: Value): Value = (u, v) match {
      case (Function(f), Function(g)) =>
        (x: Value) => (dx: Value) => diff(f(apply(dx, x)), g(x))

      case (vNew, vOld) =>
        Right(vNew)
    }

    def apply(dv: Value, v: Value): Value = (v, dv) match {
      // replacement-values always work for base-type values
      case (v, Right(vNew)) => vNew

      // Δ (Map κ τ) = Map κ ((Unit ⊎ τ) ⊎ Δτ) ⊎ Map κ τ
      //                      del  ins  modify  replace
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

      // Δ (σ ⊎ τ) = (Δσ ⊎ Δτ) ⊎ (σ ⊎ τ)
      //              modify     replace
      case (Left(v), Left(Left(dv))) =>
        Left(apply(dv, v))
      case (Right(v), Left(Right(dv))) =>
        Right(apply(dv, v))

      case (Function(f), Function(df)) =>
        (x: Value) => apply(df(x)(diff(x, x)),  f(x))
    }
  }

  override def evalConst(c: Constant): Value = c match {
    case Diff =>
      (u: Value) => (v: Value) => Value.diff(u, v)

    case Apply =>
      (dv: Value) => (v: Value) => Value.apply(dv, v)

    case EmptyMap =>
      ValueMap()

    case Update =>
      (k: Value) => (v: Value) => (m: Value) =>
        m.toMap.updated(k, v)

    case Lookup =>
      (k: Value) => (m: Value) => m.toMap.withDefault(
        k => throw new
          java.util.NoSuchElementException(
            "key " ++ k.toString ++
            " not found in " ++ m.toMap.toString)
      )(k)

    case Fold =>
      (f: Value) => (z: Value) => (map: Value) =>
        map.toMap.foldRight(z)((p, b) => f(p._1)(p._2)(b))

    case Left =>
      (x: Value) => Value.Left(x)

    case Right =>
      (y: Value) => Value.Right(y)

    case Either =>
      (f: Value) => (g: Value) => (s: Value) =>
        s.toSum.fold(x => f(x), y => g(y))

    case _ =>
      super.evalConst(c)
  }
}
