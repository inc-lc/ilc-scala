package ilc
package howTo

trait GetConstraintsOutOfConstants
extends feature.base.Syntax
{
  // language features of ILC often have polymorphic constants.
  // for example, EmptyMap represents a family of empty maps,
  // one for each pair of key-value types. In Haskell syntax:
  //
  // emptyMap : Map k v

  // from ilc.feature.maps.Syntax

  case class MapType(keyType: Type, valType: Type) extends Type

  object EmptyMap extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valType") {
      case Seq(keyType, valType) =>
        MapType(keyType, valType)
    }
  }

  // Each polymorphic constant extends a sub-trait of
  // ilc.feature.base.Syntax.PolymorphicConstant, hence
  // every polymorphic constant has a field "typeConstructor".
  // The type constructor constructs the type of a specific
  // constant when given enough types as arguments.

  case object DummyKey extends Type
  case object DummyVal extends Type

  val dummyMapType = EmptyMap.typeConstructor(Seq(DummyKey, DummyVal))

  println(s"\n\n\ndummyMapType = $dummyMapType\n\n")
  
  // running in sbt
  //
  // > test-only ilc.howTo.GetC*
  //
  // should produce, among other things,
  //
  //   dummyMapType = MapType(DummyKey,DummyVal)


  // During type inference, the presence of a polymorphic constant
  // imposes some constraint on the type. For example, the type
  // of an occurrence of EmptyMap can be (Map k v) for some k, v,
  // but it can be neither a function nor a tuple. To take this
  // constraint into consideration during unification, we can
  // generate some fresh type variables and call the constant's
  // type constructor. The resultant type's overall shape will
  // be fixed, but positions taken by the fresh type variables
  // are free to unify to anything.

  object DummyNameGenerator {
    var i = 40
    def next: Int = { i += 1 ; i }
  }

  case class TypeVariable(name: Int) extends Type

  // instantiate a polymorphic constant to a concrete unknown type
  def instantiate(constant: PolymorphicConstant): Type =
    constant.typeConstructor(
      Seq.fill(constant.typeConstructor.arity) {
        TypeVariable(DummyNameGenerator.next)
      }
    )

  val freshMap = instantiate(EmptyMap)

  println(s"freshMap = $freshMap\n\n")

  // > test-only ilc.howTo.getC*
  //
  // freshMap = MapType(TypeVariable(41), TypeVariable(42))
}
