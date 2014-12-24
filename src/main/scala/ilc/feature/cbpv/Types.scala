package ilc
package feature
package cbpv

trait Types extends functions.Types {
  /*
  //U
  case class UThunkType(t: Type) extends Type {
    override def toString = s"U($t)"
    override def traverse(f: Type => Type): Type =
      copy(f(t))
  }
  //F
  case class FProducerType(t: Type) extends Type {
    override def toString = s"F($t)"
    override def traverse(f: Type => Type): Type =
      copy(f(t))
  }
  */
}

trait TypeUtils {
  this: Types =>

  /*def stripFType(ct: Type): Type = ct match {
    case FProducerType(vt) => vt
    case _ => ???
  }*/
}

//For now, just the very core type language.
trait CBPVTypes {
  sealed trait ValType
  case class UThunkVT(t: CompType) extends ValType
  case object UnitVT extends ValType
  case class ProdVT(a: ValType, b: ValType) extends ValType

  //The original language has indexed sums, not binary ones.
  case class SumVT(a: ValType, b: ValType) extends ValType

  sealed trait CompType
  case class FProducerCT(t: ValType) extends CompType
  case class FunCT(s: ValType, t: CompType) extends CompType

  //The original language has indexed products, not binary ones.
  case class ProdCT(a: CompType, b: CompType) extends CompType
}

trait TypeConversions extends CBPVTypes with Types with unit.Types with sums.Types with products.Types with booleans.Types {
  def cbnTypeToCBPV(t: Type): CompType = t match {
    case s =>: t =>
      FunCT(UThunkVT(
        cbnTypeToCBPV(s)),
        cbnTypeToCBPV(t))
    case ProductType(a, b) =>
      ProdCT(
        cbnTypeToCBPV(a),
        cbnTypeToCBPV(b))
    case SumType(a, b) =>
      FProducerCT(SumVT(
        UThunkVT(cbnTypeToCBPV(a)),
        UThunkVT(cbnTypeToCBPV(b))))
    case UnitType =>
      FProducerCT(UnitVT)
    case BooleanType =>
      FProducerCT(SumVT(UnitVT, UnitVT))
  }

  def cbvTypeToCBPV(t: Type): ValType = t match {
    case s =>: t =>
      UThunkVT(FunCT(cbvTypeToCBPV(s), FProducerCT(cbvTypeToCBPV(t))))
    case ProductType(a, b) =>
      ProdVT(cbvTypeToCBPV(a), cbvTypeToCBPV(b))
    case SumType(a, b) =>
      SumVT(cbvTypeToCBPV(a), cbvTypeToCBPV(b))
    case UnitType =>
      UnitVT
    case BooleanType =>
      SumVT(UnitVT, UnitVT)
  }

  /*
  def compTypeToType(t: CompType): Type = t match {
    case FProducerCT(t) => FProducerType(valTypeToType(t))
    case FunCT(s, t) =>
      valTypeToType(s) =>: compTypeToType(t)
    //XXX We seem to erase the difference between the two different product types.
    case ProdCT(a, b) =>
      ProductType(compTypeToType(a), compTypeToType(b))
  }

  def valTypeToType(t: ValType): Type = t match {
    case UThunkVT(t) => UThunkType(compTypeToType(t))
    case UnitVT => UnitType
    case SumVT(a, b) => SumType(valTypeToType(a), valTypeToType(b))
    case ProdVT(a, b) => ProductType(valTypeToType(a), valTypeToType(b))
  }
  */

  def stripUVal(vt: ValType): CompType = vt match {
    case UThunkVT(ct) => ct
    case _ => ???
  }

  def stripFComp(ct: CompType): ValType = ct match {
    case FProducerCT(vt) => vt
    case _ => ???
  }
}
