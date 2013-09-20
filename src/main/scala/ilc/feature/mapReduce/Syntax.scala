package ilc
package feature
package mapReduce

trait Syntax
extends maps.Syntax
   with maps.Types
   with maybe.Types
   with products.Types
{
  // intro/elim forms of maps for mapReduce (of base-type values)
  //
  // (new constructors)
  //
  //   singleton : k → v → Map k v
  //
  //   combine : (v → v → Maybe v) → Map k v → Map k v
  //
  //   foldGroup : (b × (b → b → b) × (b → b)) → (k → a → b) → Map k a → b
  //
  // (It is not possible to encode combine with foldGroup because
  // there is no other means to enlarge the map.)
  //
  // (inherited from feature.maps)
  //
  //   empty : Map k v
  //   delete : k → Map k v → Map k v
  //   lookup : k → Map k v → Maybe v

  object Singleton extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        keyType =>: valType =>: MapType(keyType, valType)
    }
  }

  object Combine extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("k", "v") {
      case Seq(k, v) =>
        (v =>: v =>: MaybeType(v)) =>: MapType(k, v) =>: MapType(k, v)
    }
  }

  //object FoldGroup extends ConstantWith3TypeParameters
}
