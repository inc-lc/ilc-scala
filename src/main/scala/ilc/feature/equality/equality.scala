package ilc
package feature
package equality

trait Syntax extends base.Syntax with sums.Types with unit.Types {
  val Bool = SumType(UnitType, UnitType)
  //Signature: v -> v -> Bool
  case object Eq extends ConstantWith1TypeParameter {
    override val typeConstructor = TypeConstructor("v") { v => Bool }
  }
}

trait Library {
  type ObjBool = Either[Unit, Unit]

  //Do we have any encoding of Booleans to follow?
  def boolToObjBool(b: Boolean): ObjBool =
    if (b)
      Left(())
    else
      Right(())

  def equal[T](a: T, b: T): ObjBool =
    boolToObjBool(a == b)
}

trait ToScala extends base.ToScala with Syntax {
  override def toScala(t: Term): String =
    t match {
      case Eq(v) =>
        // XXX We should typecase here.
        "equal[${toScala(v)] _"
      case _ =>
        super.toScala(t)
    }

}
