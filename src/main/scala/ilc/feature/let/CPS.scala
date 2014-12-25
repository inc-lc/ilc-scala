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
  def cpsMonad(t: Type) =
    (t =>: AnswerT) =>: AnswerT

  //def argToCPS(tau: Type): Type = tau traverse toCPST
  //def retToCPS(tau: Type): Type = ???

  def toCPSTRec(tau: Type): Type =
    tau traverse toCPSTRec match {
      case s =>: t =>
        //toCPST(s) =>: toCPST(t)
        s =>: cpsMonad(t)
        //retToCPS(t)
      case t =>
        t
        //cpsMonad(t)
    }
  def toCPST(tau: Type): Type =
    cpsMonad(toCPSTRec(tau))

  def toCPSContType(t: Type) = {
    //(t =>: AnswerT)
    val v = toCPST(t)
    println(v)
    v match {
      case src =>: dst =>
        src
    }
  }

  def kVar(t: Type): Var = Var(freshName("k"), t)

  def toCPSU: Term => UntypedTerm = {
    case t @ App(f, arg) =>
      /*lambda(kVar(t))(k =>
        toCPS(f) ! lambda(Var("a", f.getType))(a =>
          toCPS(arg) ! lambda(Var("b", arg.getType))(b =>
            a ! b ! k)))*/
      val kV = freshName("k").toString
      val aV = freshName("a").toString
      val bV = freshName("b").toString
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
      val k = freshName("k").toString
      UAbs(k, None,
          UApp(k, UAbs(v.getName.toString, None, toCPSU(body))))
    case Var(name, _) =>
      val k = freshName("k").toString
          //toCPSContType(value.getType)
      UAbs(k, None, UApp(k, name.toString))
  }
  def toCPS: Term => Term = {
    case t @ App(f, arg) =>
      /*lambda(kVar(t))(k =>
        toCPS(f) ! lambda(Var("a", f.getType))(a =>
          toCPS(arg) ! lambda(Var("b", arg.getType))(b =>
            a ! b ! k)))*/
      val kV = kVar(toCPST(t.getType))
      val aV = Var(freshName("a"), toCPSContType(f.getType))
      val bV = Var(freshName("b"), toCPSContType(arg.getType))
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
      val k = kVar(toCPST(t.getType))
      Abs(k, App(k, Abs(v, toCPS(body))))
    case value =>
      val k = kVar(value.getType =>: AnswerT)
          //toCPSContType(value.getType)
      Abs(k, App(k, value))
  }
}