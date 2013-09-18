package ilc
package examples

import language.bacchus

trait MapSucc
extends Archive
{
  // Example 3 variant: mapping over the values of a map
  // program = map succ
  // input = Map(1 -> 1, 2 -> 2, 3 -> 3)
  // program input = Map(1 -> 2, 2 -> 3, 3 -> 4)
  //
  // The names of examples can be used elsewhere to access
  // a particular example. Rename wisely.
  class MapSuccExample
  extends Example
     with bacchus.Syntax
     with bacchus.ToScala
  {

    def program: Term = ???
    //map(nat, nat)(const(succ))

    override def derivative: Term = ??? /*
        dmap_s1(
          const(succ),
          const(TypedAbs("_", dnat, dsucc))) */

    def succ: Term = Plus ! 1

    // typed version of derivePlus(succ)
    def dsucc: Term = ??? /*
      TypedAbs("_", nat,
        TypedEither(UnitType, nat, dnat)(
          TypedAbs("_", UnitType, TypedLeft(UnitType, nat)(UnitTerm)))(
          TypedAbs("new", nat, TypedRight(UnitType, nat)(succ("new"))))) */

    // ignores an argument of type nat
    def const(f: Term): Term = ??? /*{
      val dontcare = uniqueName(f, "_")
      TypedAbs(dontcare, nat, f)
    }*/

    // specialize to mapping from to values of the same type
    // to save one type param. encoded using a fold.
    //
    // map : (k → v → v) → Map k v → Map k v
    //
    def map(keyType: Type, valType: Type): Term = ??? /*{
      val mapType = MapType(keyType, valType)
      TypedAbs("f", keyType =>: valType =>: valType,
        TypedFold(keyType, valType, mapType)(
          TypedAbs("k", keyType, TypedAbs("v", valType,
            TypedAbs("acc", mapType,
              TypedUpdate(keyType, valType)(
                "k")(Var("f")("k")("v"))("acc")))))(
          TypedEmptyMap(keyType, valType)))
    }*/

    // hand-crafted derivative of map with the first argument stable
    // and with the type parameters specialized to:
    // f: nat → nat → nat
    def dmap_s1(f: Term, df: Term): Term = ??? /*TypedAbs("m", mtype,
      TypedEither(dmtype.leftType, dmtype.rightType, dmtype)(
        // if the change is insertion/deletion/modification,
        // then map over the changes and see what happens.
        TypedAbs("idm", dmtype.leftType,
          TypedLeft(dmtype.leftType, dmtype.rightType)(
            map(nat, idmtype)(TypedAbs("k", nat,
              TypedEither(idmtype.leftType, idmtype.rightType, idmtype)(
                // case del/ins
                TypedEither(UnitType, nat, idmtype)(
                  // case deletion: delete it anyway
                  TypedAbs("_", UnitType,
                    TypedLeft(idmtype.leftType, idmtype.rightType)(
                      TypedLeft(UnitType, nat)(UnitTerm)))
                )(
                  // case insertion: insert the new one
                  TypedAbs("new", nat,
                    TypedLeft(idmtype.leftType, idmtype.rightType)(
                      TypedRight(UnitType, nat)(f("k")("new"))))
                )
              )(
                // case update:
                // 1. apply the given derivative `df`
                // 2. wrap result change in an ins/del/mod type
                TypedAbs("dn", dnat,
                  TypedRight(idmtype.leftType, idmtype.rightType)(
                    df("k")(TypedRight(UnitType, nat)("k"))(
                      TypedEither(UnitType, nat, nat)(
                        // case not found
                        // we need an Error term that generates
                        // code that throws an error when executed,
                        // to replace 1997.
                        TypedAbs("_", UnitType, 1997)
                      )(
                        // case found
                        TypedAbs("x", nat, "x")
                      )(
                        TypedLookup(nat, nat)("k")("m")
                      )
                    )("dn")))
              ))
            )("idm")))
      )(
        // if the change is a replacement,
        // then increment the replacement.
        TypedAbs("rep", mtype,
          TypedRight(dmtype.leftType, dmtype.rightType)(
            map(nat, nat)(f)("rep"))))) */

    // aliases
/*
    def nat = NatType
    def dnat = NatType
    def fold = TypedFold(NatType, NatType, MapType(NatType, NatType))
    def mtype = MapType(NatType, NatType)
    def empty = TypedEmptyMap(NatType, NatType)
    def dmtype = SumType(MapType(nat, idmtype), mtype)
    val idmtype = SumType(SumType(UnitType, nat), dnat) */
  }

  // the compiled object is "MapSuccBinary"
  // the benchmarking object is "MapSuccBenchmark"
  // TODO: UNCOMMENT ME!
  //addExample("MapSucc", new MapSuccExample)
}
