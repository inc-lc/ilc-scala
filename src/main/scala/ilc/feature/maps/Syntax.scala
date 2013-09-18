package ilc
package feature
package maps

trait Syntax
extends base.Syntax
   with maps.Types
   with maybe.Types
{
  // intro/elim forms of maps (of base-type values)
  //
  //   empty : Map k v
  //   update : k → v → Map k v → Map k v
  //   delete : k → Map k v → Map k v
  //   lookup : k → Map k v → Maybe v
  //   fold : (k → a → b → b) → b → Map k a → b
  //
  object EmptyMap extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        MapType(keyType, valType)
    }
  }

  object Update extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) => {
        val mapType = MapType(keyType, valType)
        keyType =>: valType =>: mapType =>: mapType
      }
    }
  }

  object Lookup extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        keyType =>: MapType(keyType, valType) =>: MaybeType(valType)
    }
  }

  object Delete extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        keyType =>: MapType(keyType, valType) =>: MapType(keyType, valType)
    }
  }

  object Fold extends ConstantWith3TypeParameters {
    val typeConstructor = TypeConstructor("k", "a", "b") {
      case Seq(k, a, b) =>
        (k =>: a =>: b =>: b) =>: b =>: MapType(k, a) =>: b
    }
  }
}

trait SyntaxSugar
extends Syntax
  with functions.Syntax
  with maybe.Syntax
{
  def mapLiteral[K <% Term, V <% Term]
    (keyValuePair: (K, V), otherPairs: (K, V)*): Term =
  {
    val (keyType, valueType) =
      (keyValuePair._1.getType, keyValuePair._2.getType)

    def loop(keyValuePairs: Seq[(K, V)]): Term =
      if (keyValuePairs.isEmpty)
        EmptyMap(keyType, valueType)
      else {
        val key: Term = keyValuePairs.head._1
        val value: Term = keyValuePairs.head._2
        Update ! key ! value ! loop(keyValuePairs.tail)
      }

    loop(keyValuePair +: otherPairs)
  }

  val mapMinus: PolymorphicTerm = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term = argumentTypes match {
      case Seq(MapType(keyType, valueType), MapType(keyType2, valueType2))
          if keyType == keyType2 && valueType == valueType2 => {
        val mapType = MapType(keyType, valueType)
        lambda(mapType, mapType) { case Seq(minuend, subtrahend) =>
          Fold !
            lambda(
              Var("key", keyType),
              Var("value", valueType),
              Var("wipMap", mapType)
            ) {
              case Seq(key, value, wipMap) =>
                Maybe !
                  (Update ! key ! value ! wipMap) !
                  lambda(valueType) {dontcare => wipMap} !
                  (Lookup ! key ! subtrahend)
            } !
            (EmptyMap of mapType) !
            minuend
        }
      }

      case _ =>
        typeErrorNotTheSame("subtraction between maps",
          "two maps of the same type",
          argumentTypes)
     }
  }
}
