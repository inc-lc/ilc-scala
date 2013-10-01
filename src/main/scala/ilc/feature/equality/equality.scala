package ilc
package feature
package equality

trait Syntax extends base.Syntax with sums.Types with unit.Types {
  val BoolType = SumType(UnitType, UnitType)
  //Signature: v -> v -> Bool
  case object Eq extends ConstantWith1TypeParameter {
    override val typeConstructor = TypeConstructor("v") { v =>
      v =>: v =>: BoolType
    }
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
  //This accepts arbitrary instances of Type, not just ours; see pattern match
  //below for rationale.
  private[this] def containsFunctions(t: base.Types#Type): Boolean =
    t match {
      case _ =>: _ => true
      case _ =>
        t.productIterator.toSeq exists {
          //This pattern match cannot distinguish between "our" type and another
          //one - this is visible because matching against `Type` would give an
          //unchecked warning. However, the code is in fact safe, because it's
          //enough to test for base.Types#Type.
          case x: base.Types#Type =>
            containsFunctions(x)
          case _ =>
            false
        }
    }

  override def toScala(t: Term): String =
    t match {
      case Eq(v) if !containsFunctions(v) =>
        "equal[${toScala(v)] _"
      case Eq(v) =>
        sys.error(s"Cannot implement equality on type $v containing a function type")
      case _ =>
        super.toScala(t)
    }

}
