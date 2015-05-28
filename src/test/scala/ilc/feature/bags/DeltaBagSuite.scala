package ilc
package feature
package bags

import org.scalatest.FunSuite
import org.scalatest.Matchers

class DeltaBagSuite
extends FunSuite with Matchers
{
  import integers.Library._
  import Library._

  def sum(b: Bag[Int]) =
    bagFoldGroup[Int, Int](additiveGroupOnIntegers)(x => x)(b)
  val bag_1_1 = bagUnion(bagSingleton(1))(bagSingleton(1))
  test("sum(Bag(1, 1)) = 2") {
    sum(bag_1_1) should be (2)
  }
  test("sum(Bag(1, 1, 10)) = 12") {
    val bag1_1_10 = bagUnion(bag_1_1)(bagSingleton(10))
    sum(bag1_1_10) should be (12)
  }
  val bagNeg = bagNegate(bag_1_1)
  test("negate works") {
    bagNeg apply 1 should be (-2)
  }
  test("sum . negate works") {
    sum(bagNeg) should be (-2)
  }
}
