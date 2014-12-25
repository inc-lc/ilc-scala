package ilc
package feature
package let

//An implementation of Plotkin-Fisher's CBV CPS transformation.

trait CPSTypes extends base.Types {
  case object AnswerT extends Type
}

trait CPS extends functions.Syntax with CPSTypes {
  def toCPST(t: Type) = (t =>: AnswerT) =>: AnswerT
  def kVar(t: Term): Var = Var("k", toCPST(t.getType))

  def toCPS: Term => Term = {
    case t @ App(f, arg) =>
      lambda(kVar(t))(k =>
        toCPS(f) ! lambda(Var("a", f.getType))(a =>
          toCPS(arg) ! lambda(Var("b", arg.getType))(b =>
            a ! b ! k)))
    /*
     * Plotkin's paper has three separate identical cases. However, they are
     * the same (ahem, save the recursive visit...), and once one looks at how
     * one does a CPS transformation by going
     * to CBPV first, that becomes obvious: computations need to be
     * sequentialized, while values simply need to be turned into trivial
     * computations.
     */
    case t @ Abs(v, body) =>
      val k = kVar(t)
      Abs(k, App(k, toCPS(body)))
    case value =>
      val k = kVar(value)
      Abs(k, App(k, value))
  }
}