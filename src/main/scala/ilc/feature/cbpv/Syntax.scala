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
  protected[cbpv] val freshGen = new base.FreshGen { lazy val syntax: outer.type = outer }
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

trait CBPVNbE extends SyntaxConversions {
  import freshGen.freshName
  /*
  //Attempt 1:
  def cbpvToCbpvForTypeVal(vt: ValTerm, vTyp: ValType): ValTerm = {
    vt
  }
  */

  /*
  def cbpvToCbpvForTypeComp(ct: CompTerm, cTyp: CompType): CompTerm = {
    cTyp match {
      case FProducerCT(UThunkVT(cTypInner)) =>
        ReturnCT(ThunkVT(cbpvToCbpvForTypeComp(ct, cTypInner)))
      case FProducerCT(vTyp) =>
        //XXX, no, insert a return and a bind. Optimizing:
        ct match {
          case ReturnCT(vt) =>
            ReturnCT(cbpvToCbpvForTypeVal(vt, vTyp))
          case AbsCT(_, _) =>
            sys error "impossible"
          case AppCT(funCT, argVT) =>
            ???
          case BindCT(e1, n, e2) =>
            ???
          case ForceCT(vt) =>
            //cTyp = F vTyp
            //ct = force vt
            //vt: U (ct.compType)
            ForceCT(cbpvToCbpvForTypeVal(vt, UThunkVT(cTyp)))
        }
      case FunCT(s, t) =>
        ct match {
          case AbsCT(v, body) =>
            AbsCT(v, cbpvToCbpvForTypeComp(body, t))
          case _ =>
            val v = VarVT(freshName("eta"), s)
            // Do I need to convert `ct` again here?
            AbsCT(v, cbpvToCbpvForTypeComp(AppCT(ct /* ??? */ , v), t))
        }
      case ProdCT(a, b) =>
        ???
    }
    // Too hard maybe. But the idea was to do this in a type-directed way — matching the actual and expected type!
  }
  */

  /*
  def cbpvToCbpvForTypeComp(ct: CompTerm, cTyp: CompType): CompTerm = {
    // Too easy — we must match structurally the types, because the mismatch might occur in a nested position.
    if (cTyp == ct.compType)
      ct
    else if (cTyp == FProducerCT(UThunkVT(ct.compType)))
      ReturnCT(ThunkVT(cbpvToCbpvForTypeComp(ct, ct.compType)))
    else if (ct.compType == FProducerCT(UThunkVT(cTyp))) {
      val freshVar = VarVT(freshName("eta"), UThunkVT(cTyp))
      BindCT(ct, freshVar, ForceCT(freshVar))
    } else {
      ???
      //sys error "Hopefully impossible"
    }
  }
  */

  sealed trait Value
  case class Thunk(cv: CompValue) extends Value
  case class ResidualVar(v: VarVT) extends Value

  sealed trait CompValue
  case class FunVal(v: VarVT, fun: Value => CompValue) extends CompValue
  case class Return(v: Value) extends CompValue
  sealed trait NeutralCompValue extends CompValue
  case class CompTermVal(ct: CompTerm) extends NeutralCompValue
  case class ResidualApp(funCT: NeutralCompValue, argVT: Value) extends NeutralCompValue
  case class ResidualBind(cv1: NeutralCompValue, v: VarVT, cv2Fun: Value => CompValue) extends NeutralCompValue
  case class ResidualForce(vt: Value) extends NeutralCompValue

  def eval(vt: ValTerm, env: Map[Name, Value]): Value = {
    vt match {
      case VarVT(n, _) => env(n)
      case ThunkVT(ct) =>
        Thunk(compute(ct, env))
    }
  }

  def compute(ct: CompTerm, env: Map[Name, Value]): CompValue = {
    ct match {
      case AbsCT(v, body) =>
        FunVal(v, arg => compute(body, env + (v.n -> arg)))
      case AppCT(fun, arg) =>
        val fCV = compute(fun, env)
        val argV = eval(arg, env)
        fCV match {
          case FunVal(v, funScala) =>
            funScala(argV)
          case ncv: NeutralCompValue =>
            ResidualApp(ncv, argV)
          case Return(_) =>
            sys error "type error"
        }
      case BindCT(ct1, v, ct2) =>
        val cv1 = compute(ct1, env)
        cv1 match {
          case Return(v1) =>
            compute(ct2, env + (v.n -> v1))
          case ncv: NeutralCompValue =>
            ResidualBind(ncv, v, arg => compute(ct2, env + (v.n -> arg)))
          case FunVal(v, funScala) =>
            sys error "type error"
        }
      case ReturnCT(vt) =>
        val v = eval(vt, env)
        Return(v)
      case ForceCT(vt) =>
        val v = eval(vt, env)
        v match {
          case Thunk(c) =>
            c
          case _ =>
            ResidualForce(v)
        }
    }
  }

  def reifyVal(v: Value): ValTerm = v match {
    case Thunk(cv) => ThunkVT(reifyComp(cv))
    case ResidualVar(v) => v
  }

  def reifyComp(cv: CompValue): CompTerm = cv match {
    case FunVal(v, fun) => AbsCT(v, reifyComp(fun(ResidualVar(v))))
    case Return(v) => ReturnCT(reifyVal(v))
    case ResidualApp(funCT, argVT) => AppCT(reifyComp(funCT), reifyVal(argVT))
    case ResidualBind(cv1, v, cv2Fun) => BindCT(reifyComp(cv1), v, reifyComp(cv2Fun(ResidualVar(v))))
    case ResidualForce(v) => ForceCT(reifyVal(v))
    case CompTermVal(ct) => ct
  }
}
