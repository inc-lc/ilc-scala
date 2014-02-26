package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class MapSuite
extends FlatSpec
   with Matchers
   with Inference
   with ilc.feature.maps.Syntax
   with Integers
{
  val (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) = (TypeVariable(0), TypeVariable(1), TypeVariable(2), TypeVariable(3), TypeVariable(4), TypeVariable(5), TypeVariable(6), TypeVariable(7), TypeVariable(8), TypeVariable(9))

  "Empty map" should "have type schema (Map k v)" in {
    val empty = UPConstant2(EmptyMap)
    assert(collectConstraints(empty) === ((TPConstant2(EmptyMap, MapType(t1, t2)), emptyConstraintSet)))
  }

  "Update" should "have type k → v → Map k v → Map k v" in {
    val update = UPConstant2(Update)
    assert(collectConstraints(update) === ((TPConstant2(Update, t3 =>: t4 =>: MapType(t3, t4) =>: MapType(t3, t4)), emptyConstraintSet)))
  }

  it should "partially applied to one integer should have type a -> Map Int a -> Map Int a" in {
    val i42 = UTerm(LiteralInt(42))
    val update = UPConstant2(Update)
    val (tt, c) = collectConstraints(UApp(update, i42))
    val solved = unification(c)
    val finalTerm = substitute(tt, solved)
    assert(finalTerm.getType === (t6 =>: MapType(IntType, t6) =>: MapType(IntType, t6)))
  }

  it should "partially applied to two integers should have type Map Int Int -> Map Int Int" in {
    val i42 = UTerm(LiteralInt(42))
    val i43 = UTerm(LiteralInt(43))
    val update = UPConstant2(Update)
    val (tt, c) = collectConstraints(UApp(UApp(update, i42), i43))
    val solved = unification(c)
    val finalTerm = substitute(tt, solved)
    assert(finalTerm.getType === (MapType(IntType, IntType) =>: MapType(IntType, IntType)))
  }

  it should "have the type Map Int Int when applied to two integers and the empty map" in {
    val i42 = LiteralInt(42)
    val i43 = LiteralInt(43)
    val empty = EmptyMap
    val update = Update
    val (tt, c) = collectConstraints(UApp(UApp(UApp(update, i42), i43), empty))
    val solved = unification(c)
    val finalTerm = substitute(tt, solved)
    assert(finalTerm.getType === MapType(IntType, IntType))
  }
}
