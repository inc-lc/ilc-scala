package ilc
package examples

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/** test that map-reduce terms are well typed */
class MapReduceSuite
extends FunSuite
   with ShouldMatchers
   with MapReduce
{
  case object K1 extends Type
  case object K2 extends Type
  case object V1 extends Type
  case object V2 extends Type
  case object V3 extends Type

  val mapPerKeyType =
    AbelianGroupType(V1) =>:
    (K1 =>: V1 =>: BagType(ProductType(K2, V2))) =>:
    MapType(K1, V1) =>: BagType(ProductType(K2, V2))

  test("mapPerKey is well-typed") {
    (mapPerKey ofType mapPerKeyType).getType should be(mapPerKeyType)
  }

  val groupByKeyType =
    BagType(ProductType(K2, V2)) =>: MapType(K2, BagType(V2))

  test("groupByKey is well-typed") {
    (groupByKey ofType groupByKeyType).getType should be(groupByKeyType)
  }

  val reducePerKeyType =
    AbelianGroupType(V3) =>:
    (K2 =>: BagType(V2) =>: V3) =>:
    MapType(K2, BagType(V2)) =>: MapType(K2, V3)

  test("reducePerKey is well-typed") {
    (reducePerKey ofType reducePerKeyType).getType should
      be(reducePerKeyType)
  }

  val mapReduceType =
    AbelianGroupType(V1) =>:
    AbelianGroupType(V3) =>:
    (K1 =>: V1 =>: BagType(ProductType(K2, V2))) =>:
    (K2 =>: BagType(V2) =>: V3) =>:
    MapType(K1, V1) =>: MapType(K2, V3)

  test("mapReduce is well-typed") {
    (mapReduce ofType mapReduceType).getType should be(mapReduceType)
  }
}
