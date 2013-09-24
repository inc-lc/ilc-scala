package ilc
package feature
package bags

trait Syntax extends base.Syntax with bags.Types {
  // intro/elim forms of bags (of values comparable for equality)
  //
  //   empty     : Bag v
  //   singleton : v -> Bag v
  //   union     : Bag v -> Bag v -> Bag v
  //   negate    : Bag v -> Bag v
  // In principle:
  //   foldGroup : Group b -> (v -> b) -> Bag v -> b
  // In practice:
  //   foldGroup : (op : b -> b -> b) -> (inv : b -> b) -> (e : b) -> (v -> b) -> Bag v -> b

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
        (b =>: b =>: b) =>: (b =>: b) =>: b =>: (v =>: b) =>: BagType(v) =>: b
    }
  }
}

trait SyntaxSugar extends Syntax with functions.Syntax {
  //Assumes v = u.
  /*def flatMap(v: Type) = FoldGroup(BagType(v), v) !
    Union(v) ! Negate(v) ! EmptyBag(v)
   */

  //flatMap : (v -> Bag u) -> Bag v -> Bag u
  val flatMap : PolymorphicTerm =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes.head match {
          case fType @ (v =>: BagType(u)) =>
            FoldGroup(BagType(u), v) !
              Union(u) ! Negate(u) ! EmptyBag(u)
        }
    }

  //% (first type) is enough. XXX explain

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
