package ilc
package feature
package let

trait ToHaskell {
  outer =>

  val syntax: inference.LetUntypedSyntax with base.Syntax with IsAtomic
  import syntax._

  def untypedToHaskell: UntypedTerm => String = {
    case UVar(name) =>
      name.toString
    case UApp(s, t) =>
      s"${untypedToHaskell(s)} ${untypedToHaskell(t)}"
    case UAbs(v, None, body) =>
      s"\\ $v -> ${untypedToHaskell(body)}"
    case ULet(v, exp, body) =>
      //Consider creating top-level definitions when exp is closed or when the
      //let is at the top-level.
      s"let $v = ${untypedToHaskell(exp)} in ${untypedToHaskell(body)}"
    case UPolymorphicConstant(pConst) =>
      toHaskell(pConst)
    case UMonomorphicConstant(mConst) =>
      toHaskell(mConst)
  }

  def toHaskell(pConst: PolymorphicConstant): String = pConst match {
    case _ =>
      sys error s"Unknown polymorphic constant $pConst"
  }

  def toHaskell(t: Term): String = {
    assert(isConst(t))
    t match {
      case _ =>
        sys error s"Unknown term $t"
    }
  }
}
