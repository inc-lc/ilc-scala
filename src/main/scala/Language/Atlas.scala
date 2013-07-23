/**
 * A description of the calculus Atlas
 *
 * Agda original:
 * https://github.com/ps-mr/ilc/blob/9aafd0c2835ff027b57e44ed2930f4f57147e0de/agda/Syntax/Language/Atlas.agda
 */

package Language

object Atlas extends Syntax.Lambda {

  sealed trait Constant
  case object True  extends Constant
  case object False extends Constant

  def deriveConst(c: Constant): Term = c match {
    case True  => Const(False)
    case False => Const(False)
  }
}
