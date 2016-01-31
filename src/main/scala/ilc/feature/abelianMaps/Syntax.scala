package ilc
package feature
package abelianMaps

trait Syntax
extends maps.Syntax
   with abelianGroups.Types
{
  // intro/elim forms of maps with abelian groups on values
  //
  // (new constructors)
  //
  //   singleton : k → v → Map k v
  //
  //   liftGroup : AbelianGroup v → AbelianGroup (Map k v)
  //
  //   foldByHom : AbelianGroup a → AbelianGroup b →
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
        AbelianGroupType(valType) =>:
          AbelianGroupType(MapType(keyType, valType))
    }

    /** Give only the key type of LiftGroup, deduce valType from
      * argument
      * {{{
      * (LiftGroup(Word) ! additiveGroup).getType =
      *   AbelianGroupType(MapType(Word, IntType))
      * }}}
      * Although this pattern of partially specifying type
      * arguments might be abstracted in traits, it's probably
      * more productive to simply code unification (in Haskell,
      * it's just a couple dozen lines).
      */
    def tapply(keyType: Type): PolymorphicTerm = new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term = {
        val Seq(valType) =
          TypeConstructor("v")(AbelianGroupType.apply _).
            inverse(argumentTypes.head)
        LiftGroup.tapply(keyType, valType)
      }
    }
  }

  object FoldByHom extends ConstantWith3TypeParameters {
    val typeConstructor = TypeConstructor("k", "a", "b") {
      case Seq(k, a, b) =>
        AbelianGroupType(a) =>: AbelianGroupType(b) =>:
          (k =>: a =>: b) =>: MapType(k, a) =>: b
    }
  }
}
