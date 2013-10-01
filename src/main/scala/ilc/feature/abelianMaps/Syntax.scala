package ilc
package feature
package abelianMaps

trait Syntax
extends maps.Syntax
   with abelians.Types
{
  // intro/elim forms of maps with abelian groups on values
  //
  // (new constructors)
  //
  //   singleton : k → v → Map k v
  //
  //   liftGroup : Abelian v → Abelian (Map k v)
  //
  //   foldByHom : Abelian a → Abelian b →
  //               (k → a → b) → Map k a → b
  //
  // (inherited from feature.maps)
  //
  //   empty : Map k v
  //   delete : k → Map k v → Map k v
  //   lookup : k → Map k v → Maybe v

  object SingletonMap extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        keyType =>: valType =>: MapType(keyType, valType)
    }
  }

  object LiftGroup extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        AbelianType(valType) =>: AbelianType(MapType(keyType, valType))
    }

    /** Give only the key type of LiftGroup, deduce valType from
      * argument
      * {{{
      * (LiftGroup(Word) ! additiveGroup).getType =
      *   AbelianType(MapType(Word, IntType))
      * }}}
      * Although this pattern of partially specifying type
      * arguments might be abstracted in traits, it's probably
      * more productive to simply code unification (in Haskell,
      * it's just a couple dozen lines).
      */
    def apply(keyType: Type): PolymorphicTerm = new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term = {
        val Seq(valType) =
          TypeConstructor("v")(AbelianType.apply _).
            inverse(argumentTypes.head)
        LiftGroup(keyType, valType)
      }
    }
  }

  object FoldByHom extends ConstantWith3TypeParameters {
    val typeConstructor = TypeConstructor("k", "a", "b") {
      case Seq(k, a, b) =>
        AbelianType(a) =>: AbelianType(b) =>:
          (k =>: a =>: b) =>: MapType(k, a) =>: b
    }
  }
}
