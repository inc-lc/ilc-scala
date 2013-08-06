package ilc
package feature.functions

/**
 * Free variables: an synthesized attribute
 */

import scala.language.implicitConversions

trait FV extends Attribution { self: Syntax =>

  implicit def attr_to_result(fv: FV_attr): Set[String] =
    fv(fv.rootTerm)

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
