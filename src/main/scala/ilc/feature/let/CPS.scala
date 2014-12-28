package ilc
package feature
package let

trait CPSTypes extends base.Types {
  case object AnswerT extends Type
}

/**
 * Implementations of several CPS transformation.
 * Among others, there are Plotkin-Fisher's CBV and Danvy-Filinski's one-pass
 * CBV transforms.
 */

//XXX: I can't import SyntaxSugar only, because it's written as a trait. If I
//want to allow importing hierarchically, I need to write the module
//differently! For modules with local state, that does make sense, but less so for
//hiding implicit conversions.
//However, I could still mix Syntax & SyntaxSugar in a subcomponent.
trait CPS extends functions.Syntax with CPSTypes with inference.PrettySyntax {
  outer =>
  private val freshGen = new base.FreshGen { val syntax: outer.type = outer }
  import freshGen.freshName

  /*
   * Transformers for types.
   */
  private def cpsNot(t: Type) =
    t =>: AnswerT
  private def cpsMonad(t: Type) =
    cpsNot(cpsNot(t))

  /**
    * Transform the value type {@code tau} to the corresponding CPS type.
    */
  private def cpsTransformValueType(tau: Type): Type =
    tau traverse cpsTransformValueType match {
      case s =>: t =>
        s =>: cpsMonad(t)
        //(t =>: AnswerT) =>: s =>: AnswerT
      case t =>
        t
    }
  private def toCPSContType(t: Type) =
    cpsNot(cpsTransformValueType(t))

  /**
    * Transform the computation type (in the sense of CBPV or Moggi's calculus)
    * {@code F tau} to the corresponding CPS type.
    */
  private def cpsTransformCompType(tau: Type): Type =
    cpsMonad(cpsTransformValueType(tau))

  /**
   * Transforms a type {@code tau} to the type of the result of the CPS transform.
   * In other words, if a term {@code term} has type {@code tau}, CPS-transforming
   * {@code term} will produce a term with type {@cpsTransformType(tau)}.
   */
  //This is an alias, just to offer a name which makes more sense for clients.
  def cpsTransformType(tau: Type) = cpsTransformCompType(tau)

  private def kVar(t: Type): Var = Var(freshName("k"), t)

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
    case t @ Abs(v, body) =>
      val k = freshName("k")
      UAbs(k, None, UApp(k,
        UAbs(v.getName, None, toCPSU(body))))
    case Var(name, _) =>
      val k = freshName("k")
      UAbs(k, None, UApp(k,
        name))
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

  //Danvy & Filinski's one-step CBV CPS transformation from Representing Control.
  //This version optionally avoids eta-redexes, like Fig. 3 in that paper.
  //Instead of duplicating most of the transformation, I've decided to abstract
  //over the difference between static continuations and dynamic ones.
  case class UntypedCont(k: Either[UntypedTerm, UntypedTerm => UntypedTerm]) {
    def apply(t: UntypedTerm): UntypedTerm = k match {
      case Left(k) => k(t)
      case Right(k) => k(t)
    }
  }

  def doCPSUntypedOnePass(t: Term)(k: UntypedCont): UntypedTerm =
    t match {
      case Abs(xV, body) =>
        val kV = freshName("k")
        k(xV.getName % cpsTransformValueType(xV.getType) ->:
          kV % toCPSContType(body.getType) ->:
          doCPSUntypedOnePass(body)(UntypedCont(Left(kV))))
      case t @ App(f, arg) =>
        val avoidEtaRedexes = true
        val tail = k match {
          case UntypedCont(Left(kT)) if avoidEtaRedexes =>
            kT
          case _ =>
            val aV = freshName("a")
            aV % cpsTransformValueType(t.getType) ->: k(aV)
        }
        doCPSUntypedOnePass(f)(UntypedCont(Right(m =>
          doCPSUntypedOnePass(arg)(UntypedCont(Right(n =>
            m(n)(tail)))))))
      case v: Var =>
        k(v.getName ofType cpsTransformValueType(v.getType))
    }

  //CPS transform for a dynamic context.
  def toCPSUntypedOnePass(t: Term) =
    ('k % toCPSContType(t.getType) ->:
      doCPSUntypedOnePass(t)(UntypedCont(Left('k)))): Term
  //CPS transform for empty context.
  def toCPSUntypedOnePassTopLevel(t: Term) = doCPSUntypedOnePass(t)(UntypedCont(Right(identity))): Term

  /*
   * Danvy & Filinski give a basic transform in Sec. 2.5, and later (in Sec.
   * 2.6) extend it to avoid producing eta-redexes for function calls in tail
   * position (that is, to be "properly tail-recursive"). The result of the
   * properly tail-recursive transform is eta-equivalent to the basic transform.
   *
   * Theorem 1 shows that the result of their basic transform, in the version
   * for a dynamic context, is beta-eta-equivalent to Plotkin-Fischer's
   * transformation. We implement the properly tail-recursive version, but since
   * its result is eta-equivalent, the result of toCPSUntypedOnePass is
   * equivalent to Plotkin-Fischer's result. Furthermore, when starting from
   * terms in normal form, all redexes in Plotkin-Fischer's result must be
   * administrative, so normalizing this result gives a term alpha-equivalent to
   * the result of the one-pass transformation.
   */

  case class Cont(k: Either[Term, Term => Term]) {
    def apply(t: Term): Term = k match {
      case Left(k) => k(t)
      case Right(k) => k(t)
    }
  }

  def doCPSOnePass(t: Term)(k: Cont): Term =
    t match {
      case Abs(xV, body) =>
        val kV = Var(freshName("k"), toCPSContType(body.getType))
        k(Abs(Var(xV.getName, cpsTransformValueType(xV.getType)),
          Abs(kV,
            doCPSOnePass(body)(Cont(Left(kV))))))
      case t @ App(f, arg) =>
        val avoidEtaRedexes = true
        val tail = k match {
          case Cont(Left(kT)) if avoidEtaRedexes =>
            kT
          case _ =>
            val aV = Var(freshName("a"),  cpsTransformValueType(t.getType))
            Abs(aV, k(aV))
        }
        doCPSOnePass(f)(Cont(Right(m =>
          doCPSOnePass(arg)(Cont(Right(n =>
            m(n)(tail)))))))
      case v: Var =>
        k(Var(v.getName, cpsTransformValueType(v.getType)))
    }

  //CPS transform for a dynamic context.
  def toCPSOnePass(t: Term): Term = {
    val kV = Var(freshName("k"), toCPSContType(t.getType))
    Abs(kV, doCPSOnePass(t)(Cont(Left(kV))))
  }
  //CPS transform for empty context.
  def toCPSOnePassTopLevel(t: Term): Term = doCPSOnePass(t)(Cont(Right(identity)))
}
