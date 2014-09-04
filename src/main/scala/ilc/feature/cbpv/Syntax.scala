package ilc
package feature
package cbpv

trait Syntax extends functions.Syntax with Types with TypeUtils {
  case class Produce(v: Term) extends Term {
    val getType: Type = FProducerType(v.getType)
  }

  //Aaargh.... names!
  //case class Bind(v: Term, )

  case class Thunk(v: Term) extends Term {
    val getType: Type = UThunkType(v.getType)
  }

  case class Force(v: Term) extends Term {
    val getType: Type =
      stripFType(v.getType)
  }
}

trait CBPVSyntax extends CBPVTypes with base.Names {
  trait CompTerm
  trait ValTerm
  case class VarVT(n: Name, t: ValType) extends ValTerm

  case class ThunkVT(c: CompTerm) extends ValTerm
  case class ForceCT(v: ValTerm) extends CompTerm
}

trait SyntaxConversions extends Syntax with CBPVSyntax with TypeConversions {
  def cbnToCBPV(t: Term): CompTerm = t match {
    case Var(n, cbnTyp) =>
      val cbpvTyp = cbnTypeToCBPV(cbnTyp)
      ForceCT(VarVT(n, stripFComp(cbpvTyp)))
    case Abs(v, body) =>
      ???
  }

  def cbvToCBPV(t: Term): ValTerm = t match {
    case Abs(v, body) =>
      ThunkVT(???)
  }
}
