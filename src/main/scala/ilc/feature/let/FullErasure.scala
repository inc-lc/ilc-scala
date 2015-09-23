package ilc
package feature
package let

trait FullErasure {
  outer =>

  val syntax: Syntax with IsAtomic with inference.LetUntypedSyntax
  import syntax._

  //XXX would need a better "everywhere" to abstract the traversal.
  //But whatever, the types are narrow enough to ensure needed recursive calls
  //are done.

  def varName(v: Var) = v.getName
  def fullErasure: Term => UntypedTerm = {
    case v: Var =>
      UVar(varName(v))
    case App(s, t) =>
      UApp(fullErasure(s), fullErasure(t))
    case Abs(v, body) =>
      UAbs(varName(v), None, fullErasure(body))
    case Let(v, exp, body) =>
      ULet(varName(v), fullErasure(exp), fullErasure(body))
    case pc: PolymorphicConstant =>
      UPolymorphicConstant(pc)
    case monoConstant if isAtomic(monoConstant) =>
      UMonomorphicConstant(monoConstant)
  }
}
