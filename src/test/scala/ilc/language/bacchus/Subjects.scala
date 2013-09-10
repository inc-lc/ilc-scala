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

  // typed code
  def constT(argType: Type, t: Term) = {
    val dontcare = uniqueName(t, "_")
    TypedAbs(dontcare, argType, t)
  }
  def constN(t: Term) = constT(NatType, t)

  val foldNatN = TypedFoldNat(NatType)
  val plus3 =
    TypedAbs("x", NatType,
      TypedAbs("y", NatType,
        TypedAbs("z", NatType, Plus("x", "y", "z"))))

  val leftNN = TypedLeft(NatType, NatType)
  val rightNN = TypedRight(NatType, NatType)
  val eitherNNN = TypedEither(NatType, NatType, NatType)

  val emptyNN = TypedEmptyMap(NatType, NatType)
  val updateNN = TypedUpdate(NatType, NatType)
  val lookupNN = TypedLookup(NatType, NatType)
  val foldNNN = TypedFold(NatType, NatType, NatType)

  val typed1234 = updateNN(1)(2)(updateNN(2)(4)(
    updateNN(3)(6)(updateNN(4)(8)(emptyNN))))

  // each element of `variousKeys` is a tuple
  // (oldKey, newKey, oldMap, newMap) such that the key-pairs
  // exhaust all possibilities of existence in the maps
  // old : a key of the old map (4)
  // new : a key of the new map (5)
  // both: a key of both maps (1)
  // none: a key of no map (9)
  val variousKeys: List[((Term, Term), (Term, Term))] =
    List.apply[(Term, Term)](
      (4, 4), // old -> old
      (4, 5), // old -> new
      (4, 1), // old -> both
      (4, 9), // old -> none
      (5, 4), (5, 5), (5, 1), (5, 9), // new  -> _
      (1, 4), (1, 5), (1, 1), (1, 9), // both -> _
      (9, 4), (9, 5), (9, 1), (9, 9)  // none -> _
    ).map(keyPair => (keyPair, (twiceMap1234, twiceMap1256)))

  // various possibilities for keys in a changing map
  val oldMap = twiceMap1234
  val newMap = twiceMap1256
  val keyInOld = Nat(4)
  val keyInNew = Nat(5)
  val keyInBoth = Nat(1)
  val keyInNone = Nat(9)
  val keyCases = List(keyInOld, keyInNew, keyInBoth, keyInNone)
}
