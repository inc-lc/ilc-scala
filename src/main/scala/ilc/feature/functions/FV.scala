package ilc
package feature.functions

/**
 * Free variables
 */

trait FV extends Attribution { self: Syntax =>

  def FV(t: Term): Set[String] = FV_attr(t)(Subterm.refl(t))

  case class FV_attr(root: Term)
  extends SyntheticAttribute[Set[String]](root) {
    def synthesize(s: Subterm): Set[String] = s.term match {
      case Const(_) =>
        Set.empty
      case Var(name) =>
        Set(name)
      case App(_, _) | Abs(_, _) =>
        s.down.map(lookup).fold(Set.empty)(_ ++ _)
    }
  }
}
