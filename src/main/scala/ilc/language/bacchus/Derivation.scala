package ilc
package language.bacchus

/**
 * Symbolic derivation for Bacchus
 */
trait Derivation extends feature.functions.Derivation { self: Syntax =>
  def deriveConst(c: Constant): Term = c match {
    case Diff | Apply => sys.error("cannot derive " ++ c.toString)
    case someOtherConstant => Diff(someOtherConstant)(someOtherConstant)
  }
}
