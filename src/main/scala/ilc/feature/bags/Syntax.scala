package ilc
package feature
package bags

trait Syntax
extends base.Syntax
   with bags.Types
   with abelianGroups.Types
{
  // intro/elim forms of bags (of values comparable for equality)
  //
  //   empty     : Bag v
  //   singleton : v → Bag v
  //   union     : Bag v → Bag v → Bag v
  //   negate    : Bag v → Bag v
  //   foldGroup : AbelianGroup b → (v → b) → Bag v → b

  case object EmptyBag extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("v")(BagType)
  }

  case object Singleton extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("v") { v =>
      v =>: BagType(v)
    }
  }

  case object Union extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("v") { v =>
      val bagType = BagType(v)
      bagType =>: bagType =>: bagType
    }
  }

  case object Negate extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("v") { v =>
      val bagType = BagType(v)
      bagType =>: bagType
    }
  }

  case object FoldGroup extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("b", "v") {
      case Seq(b, v) =>
        AbelianGroupType(b) =>: (v =>: b) =>: BagType(v) =>: b
    }
  }

  case object FreeAbelianGroup extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("v") { v =>
      AbelianGroupType(BagType(v))
    }
  }
}

trait StdLib
extends Syntax
   with inference.PrettySyntax
{
  //  empty     : Bag v
  val emptyBag: UntypedTerm = EmptyBag

  //  singleton : v → Bag v
  val singleton: UntypedTerm = Singleton

  //  union     : Bag v → Bag v → Bag v
  val union: UntypedTerm = Union

  //  negate    : Bag v → Bag v
  val negate: UntypedTerm = Negate

  //  foldGroup : AbelianGroup b → (v → b) → Bag v → b
  val foldGroup: UntypedTerm = FoldGroup

  val freeAbelianGroup: UntypedTerm = FreeAbelianGroup

  // flatMap : (v → Bag u) → Bag v → Bag u
  val flatMap: UntypedTerm = foldGroup(freeAbelianGroup)

  // map : (a -> b) -> Bag a -> Bag b
  val map: UntypedTerm = 'f ->: flatMap('x ->: singleton('f('x)))
}

trait SyntaxSugar
extends Syntax
   with functions.Syntax
   with abelianGroups.Syntax
{
  //flatMap : (v → Bag u) → Bag v → Bag u
  val flatMap : PolymorphicTerm =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes.head match {
          case fType @ (v =>: BagType(u)) =>
            FoldGroup.tapply(BagType(u), v) ! FreeAbelianGroup.tapply(u)
        }
    }

  //The first few lines are boilerplate for better type inference:
  val map : PolymorphicTerm =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes.head match {
          case fType @ (v =>: u) =>

            // Actual implementation, after the boilerplate, where all lambdas
            // are explicitly annotated.
            lambda(Var("f", fType)) { f =>
              flatMap ! {
                lambda (Var("x", v)) {
                  x => Singleton ! (f ! x) }}}}
    }
}
