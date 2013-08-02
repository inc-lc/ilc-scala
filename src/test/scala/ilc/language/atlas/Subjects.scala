package ilc
package language.atlas

/**
 * Test subjects for Atlas
 */

import ilc.language.Atlas._

trait Subjects {
  // λf. λx. f x
  val appFun = "f" ->: "x" ->: Var("f")("x")

  // λ_. t
  def constFun(t: Term) = uniqueName(t, "_") ->: t

  // λx. x
  val idFun = "x" ->: "x"

  def sum(t: Term): Term =
    Fold(constFun(Plus))(0)(t)

  val negMap1234: Term = Map(1 -> -1, 2 -> -2, 3 -> -3, 4 -> -4)

  val negMap1256: Term = Map(1 -> -1, 2 -> -2, 5 -> -5, 6 -> -6)

  val idMap2367: Term = Map(2 -> 2, 3 -> 3, 6 -> 6, 7 -> 7)

  val map1368: Term = Map(1 -> 10, 3 -> 3, 6 -> 60, 8 -> 80)

  val oddityMap1234: Term =
    Map(1 -> True, 2 -> False, 3 -> True, 4 -> False)

  val primeMap10: Term =
    Map(2 -> True, 3 -> True, 4 -> False,  5 -> True, 6 -> False,
        7 -> True, 8 -> False, 9 -> False, 10 -> False)

  val nilTerm = Empty

  val terms = List.apply[Term](
    True,
    False,
    Lookup(2)(oddityMap1234),
    1984,
    sum(negMap1256),
    negMap1234,
    primeMap10,
    Map(negMap1234 -> oddityMap1234, negMap1256 -> primeMap10))

  val intsAndInts = {
    val lhs = Array(914, 649, 869, 432, 795, 761, 1, 3, 5)
    val rhs = Array(904, 772, 178, 470, 484, 889, 2, 4, 6)
    (lhs, rhs).zipped.toList
  }
}
