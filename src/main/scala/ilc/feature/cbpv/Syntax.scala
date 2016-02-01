package ilc
package feature
package cbpv

/*
trait Syntax extends functions.Syntax with Types with TypeUtils {
  case class Return(v: Term) extends Term {
    val getType: Type = FProducerType(v.getType)
  }

  //Aaargh.... names!
  //case class Bind(e1: Term, v: Term, e2: Term)

  case class Thunk(v: Term) extends Term {
    val getType: Type = UThunkType(v.getType)
  }

  case class Force(v: Term) extends Term {
    val getType: Type =
      stripFType(v.getType)
  }
}
*/

//Let's use restricted CBPV.
//XXX the naming scheme is broken: VT and CT are also used as suffixes for value
//types.
trait CBPVSyntax extends CBPVTypes with base.Names {
  sealed trait CompTerm {
    def compType: CompType = null
  }
  sealed trait ValTerm {
    def valType: ValType = null
  }

  /////////////////////
  //   Value terms   //
  /////////////////////

  //vVar : ∀ {τ} (x : ValVar Γ τ) → Val Γ τ
  case class VarVT(n: Name, t: ValType) extends ValTerm {
    override def valType = t
  }
  //vThunk : ∀ {τ} → Comp Γ τ → Val Γ (U τ)
  case class ThunkVT(c: CompTerm) extends ValTerm {
    override lazy val valType = UThunkVT(c.compType)
  }

  // And value constants:
  //vConst : ∀ {Σ τ} →
  //  (c : ValConst Σ τ) →
  //  (args : Vals Γ Σ) →
  //  Val Γ τ

  ///////////////////////
  // Computation terms //
  ///////////////////////

  //cAbs : ∀ {σ τ} →
  //  (t : Comp (σ •• Γ) τ) →
  //  Comp Γ (σ ⇛ τ)
  case class AbsCT(n: VarVT, body: CompTerm) extends CompTerm {
    override lazy val compType = FunCT(n.valType, body.compType)
  }

  //cApp : ∀ {σ τ} →
  //  (s : Comp Γ (σ ⇛ τ)) →
  //  (t : Val Γ σ) →
  //  Comp Γ τ
  case class AppCT(f: CompTerm, arg: ValTerm) extends CompTerm {
    override lazy val compType = {
      f.compType match {
        case FunCT(s, t) if s == arg.valType =>
          t
        case _ =>
          ???
      }
    }
  }

  //_into_ : ∀ {σ τ} →
  //  (e₁ : Comp Γ (F σ)) →
  //  (e₂ : Comp (σ •• Γ) (F τ)) →
  //  Comp Γ (F τ)
  case class BindCT(e1: CompTerm, n: VarVT, e2: CompTerm) extends CompTerm {
    override lazy val compType = {
      if (e1.compType == FProducerCT(n.valType))
        e2.compType
      else
        ???
    }
  }

  //cForce : ∀ {τ} → Val Γ (U τ) → Comp Γ τ
  case class ForceCT(v: ValTerm) extends CompTerm {
    override lazy val compType = {
      v.valType match {
        case UThunkVT(ct) => ct
        case _ => ???
      }
    }
  }

  //cReturn : ∀ {τ} (v : Val Γ τ) → Comp Γ (F τ)
  case class ReturnCT(v: ValTerm) extends CompTerm {
    override lazy val compType = FProducerCT(v.valType)
  }

  // And computation constants:
  //cConst : ∀ {Σ τ} →
  //  (c : CompConst Σ τ) →
  //  (args : Vals Γ Σ) →
  //  Comp Γ τ
}

trait SyntaxConversions extends functions.Syntax with CBPVSyntax with TypeConversions {
  outer =>
  private val freshGen = new base.FreshGen { lazy val syntax: outer.type = outer }
  import freshGen.freshName

  def cbnToCBPV(t: Term): CompTerm = t match {
    case Var(n, cbnTyp) =>
      val cbpvTyp = cbnTypeToCBPV(cbnTyp)
      ForceCT(VarVT(n, stripFComp(cbpvTyp)))
    case Abs(v, body) =>
      AbsCT(VarVT(v.getName, UThunkVT(cbnTypeToCBPV(v.getType))), cbnToCBPV(body))
    case App(f, arg) =>
      AppCT(cbnToCBPV(f), ThunkVT(cbnToCBPV(arg)))
  }

  def cbvValueToCBPV(t: Term): ValTerm = t match {
    case Abs(v, body) =>
      ThunkVT(AbsCT(VarVT(v.getName, cbvTypeToCBPV(v.getType)), cbvToCBPV(body)))
    case v: Var =>
      VarVT(v.getName, cbvTypeToCBPV(v.getType))
  }

  def cbvToCBPV(t: Term): CompTerm = t match {
    case App(f, arg) =>
      val argV = VarVT(freshName("a"), cbvTypeToCBPV(arg.getType))
      val fV = VarVT(freshName("f"), cbvTypeToCBPV(f.getType))
      BindCT(cbvToCBPV(arg), argV,
        BindCT(cbvToCBPV(f), fV,
          AppCT(ForceCT(fV), argV)))
    case value =>
      ReturnCT(cbvValueToCBPV(value))
  }
}
