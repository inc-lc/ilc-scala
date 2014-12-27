package ilc
package feature
package let

//An implementation of Plotkin-Fisher's CBV CPS transformation.

trait CPSTypes extends base.Types {
  case object AnswerT extends Type
}

//XXX: I can't import SyntaxSugar only, because it's written as a trait. If I
//want to allow importing hierarchically, I need to write the module
//differently! For modules with local state, that does make sense.
trait CPS extends functions.Syntax with CPSTypes with inference.Inference /* XXX for traverse!*/ with inference.PrettySyntax {
  outer =>
  private val freshGen = new base.FreshGen { val syntax: outer.type = outer }
  import freshGen.freshName

  private def cpsNot(t: Type) =
    t =>: AnswerT
  private def cpsMonad(t: Type) =
    cpsNot(cpsNot(t))

  private def cpsTransformValueType(tau: Type): Type =
    tau traverse cpsTransformValueType match {
      case s =>: t =>
        s =>: cpsMonad(t)
        //(t =>: AnswerT) =>: s =>: AnswerT
      case t =>
        t
        //We need to know whether the context of this type is "value"
        //or "computation" — that is, indeed, the polarity of the context.
        //For a value, we should do nothing, but for a computation, we should (probably?) do something.
        //cpsMonad(t)
    }
  private def toCPSContType(t: Type) =
    cpsNot(cpsTransformValueType(t))

  //Name for internal perspective
  private def cpsTransformCompType(tau: Type): Type =
    cpsMonad(cpsTransformValueType(tau))

  def cpsTransformType(tau: Type) = cpsTransformCompType(tau)

  def kVar(t: Type): Var = Var(freshName("k"), t)

  def toCPSU: Term => UntypedTerm = {
    case t @ App(f, arg) =>
      val kV = freshName("k")
      val aV = freshName("a")
      val bV = freshName("b")
      UAbs(kV, None,
        UApp(toCPSU(f), UAbs(aV, None,
          UApp(toCPSU(arg), UAbs(bV, None,
            UApp(
              UApp(aV, bV),
              kV))))))
    /*
     * Plotkin's paper has three separate identical cases. However, they are
     * the same (ahem, save the recursive visit...), and once one looks at how
     * one does a CPS transformation by going
     * to CBPV first, that becomes obvious: computations need to be
     * sequentialized, while values simply need to be turned into trivial
     * computations.
     */
    case t @ Abs(v, body) =>
      val k = freshName("k")
      UAbs(k, None,
        UApp(k, UAbs(v.getName, None, toCPSU(body))))
    case Var(name, _) =>
      val k = freshName("k")
      UAbs(k, None, UApp(k, name))
  }

  def varTransf: Var => Var = {
    case Var(name, typ) =>
      Var(name, cpsTransformValueType(typ))
  }

  def toCPS: Term => Term = {
    case t @ App(f, arg) =>
      /* Variables 'a' and 'b' are not bound to continuations! We're building
       * continuations, so they are the continuation parameters!
       */
      /*lambda(kVar(toCPSContType(t.getType)))(k =>
        toCPS(f) ! lambda(Var("a", cpsTransformValueType(f.getType)))(a =>
          toCPS(arg) ! lambda(Var("b", cpsTransformValueType(arg.getType)))(b =>
            a ! b ! k)))*/

      val kV = kVar(toCPSContType(t.getType))
      val aV = Var(freshName("a"), cpsTransformValueType(f.getType))
      val bV = Var(freshName("b"), cpsTransformValueType(arg.getType))
      Abs(kV,
        App(toCPS(f), Abs(aV,
          App(toCPS(arg), Abs(bV,
            App(
              App(aV, bV),
              kV))))))
    /*
     * Plotkin's paper has three separate identical cases. However, they are
     * the same (ahem, save the recursive visit...), and once one looks at how
     * one does a CPS transformation by going
     * to CBPV first, that becomes obvious: computations need to be
     * sequentialized, while values simply need to be turned into trivial
     * computations.
     */
    case t @ Abs(v, body) =>
      val k = kVar(toCPSContType(t.getType))
      Abs(k, App(k, Abs(varTransf(v), toCPS(body))))
    case v: Var =>
      val k =
        kVar(toCPSContType(v.getType))
      Abs(k, App(k, varTransf(v)))
  }
}