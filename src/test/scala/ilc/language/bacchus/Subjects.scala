package ilc
package language.bacchus

import language.Bacchus._

// TODO: think about how to share test subjects across calculi.
// put some subjects in feature.functions maybe?

object Subjects {
  val primeMap10 = Map(1 -> ff, 2 -> tt, 3 -> tt, 4 -> ff, 5 -> tt,
                       6 -> ff, 7 -> tt, 8 -> ff, 9 -> ff, 10 -> ff)

  val oddityMap1234 = Map(1 -> tt, 2 -> ff, 3 -> tt, 4 -> ff)

  val twiceMap1234 = Map(1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8)

  val twiceMap1256 = Map(1 -> 200, 2 -> 4, 5 -> 10, 6 -> 12)

  val natsAndNats = {
    val lhs = Array(914, 649, 869, 432, 795, 761, 1, 3, 5)
    val rhs = Array(904, 772, 178, 470, 484, 889, 2, 4, 6)
    (lhs, rhs).zipped.toList
  }

  val idFun = "x" ->: "x"

  val fst = Lambda("x", "y") ->: "x"
  val snd = Lambda("x", "y") ->: "y"

  def sum(t: Term): Term =
    Fold(constFun(Plus))(0)(t)

  def constFun(t: Term) = uniqueName(t, "_") ->: t
}
