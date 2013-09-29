package ilc
package feature
package maybe

trait Evaluation extends functions.Evaluation with Syntax {
  case class MaybeValue(lift: Option[Value]) extends Value {
    override def toString: String =
      lift.fold("Nope")(x => s"Just($x)")
  }

  override def coreEval(t: Term, env: Env): Value = t match {
    case Maybe(_, _) =>
      (z: Value) => (f: Value) => (maybe: Value) => maybe match {
        case MaybeValue(option) =>
          option.fold(z) { x => f(x) }

        case _ =>
          maybe die " should be MaybeValue"
      }
    case Nope(_) =>
      MaybeValue(None)
    case Just(_) =>
      (value: Value) => MaybeValue(Some(value))
    case _ =>
      super.coreEval(t, env)
  }
}
