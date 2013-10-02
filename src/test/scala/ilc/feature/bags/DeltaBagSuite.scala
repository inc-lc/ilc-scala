package ilc
package feature
package bags

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class LibrarySuite
extends FunSuite with ShouldMatchers
{
  import Library._
  import abelianGroups.Library._

  def sum(b: Bag[Int]) =
    bagFoldGroup(GenerativeGroup[Int](x => x + _, -_, 0))(identity[Int])(b)
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
