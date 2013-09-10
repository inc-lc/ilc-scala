package ilc
package feature
package maps

trait Syntax extends base.Syntax {
  // intro/elim forms of maps (of base-type values)
  //
  //   empty : Map k v
  //   update : k → v → Map k v → Map k v
  //   delete : k → Map k v → Map k v
  //   lookup : k → Map k v → Maybe v
  //   fold : (k → a → b → b) → b → Map k a → b
  //
  case object EmptyMap extends Constant
  case object Update extends Constant
  case object Delete extends Constant
  case object Lookup extends Constant
  case object Fold extends Constant
}

trait SyntaxSugar extends Syntax with functions.Syntax {
  // easy construction of map literals (copied from Atlas)
  def Map(assoc: (Term, Term)*): Term =
    updatesFrom(EmptyMap, assoc: _*)

  def fromList(base: Term, assoc: List[(Term, Term)]): Term =
    assoc match {
      case Nil => base
      case (k, v) :: assoc =>
        Update(k)(v)(fromList(base, assoc))
    }

  // shorthand for chain updates
  def updatesFrom(base: Term, assoc: (Term, Term)*): Term =
    fromList(base, assoc.toList)

  // mapValues: (a -> b) -> Map k a -> Map k b
  val mapValues: Term = Lambda("f", "map") ->:
    Fold(Lambda("key", "value", "accumulator") ->:
           Update("key")(Var("f")("value"))("accumulator"))(
         EmptyMap)("map")

}
