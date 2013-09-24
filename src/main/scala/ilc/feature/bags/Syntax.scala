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
  def flatMap(v: Type) = FoldGroup(BagType(v), v) !
    Union(v) ! Negate(v) ! EmptyBag(v)

  def map(v: Type) =
    lambda("f") { f =>
      flatMap(v) ! {
        lambda (Var("x", v)) {
          x => Singleton ! (f ! x) }}}

  /*
  // Point-free version, two args are due.
  //   flatMap : (v -> Bag u) -> Bag v -> Bag u
  val flatMap = FoldGroup !
    Union ! Negate ! EmptyBag

  //Hey, careful, we have no Hindley-Milner in this world.
  //Frankly, you can't write this in Scala, either.
  val compose = lambda("f", "g", "x") { case Seq(f, g, x) => f ! (g ! x) }

  val map = lambda("f") { f => flatMap ! (compose ! Singleton ! f) }

  //Alternative?
  //def compose(f: Term, g: Term): Term = lambda("x") (x => f ! (g ! x))

  //val map = lambda("f") { f => flatMap ! compose(Singleton, f) }
   */
}
