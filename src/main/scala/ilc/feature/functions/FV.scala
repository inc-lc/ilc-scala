package ilc
package feature.functions

/**
 * Free variables: an synthesized attribute
 */

trait FV extends Attribution { self: Syntax =>

  def FV(t: Term): Set[String] = FV_attr(t)(Subterm.refl(t))

  case class FV_attr(root: Term)
  extends SynthesizedAttribute[Set[String]](root) {
    def synthesize(s: Subterm,
                   childAttr: List[Set[String]]): Set[String] =
      s.term match {
        case Var(name) =>
          Set(name)
        case _ =>
          childAttr.fold(Set.empty)(_ ++ _)
      }
  }
}
