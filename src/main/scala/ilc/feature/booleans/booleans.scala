package ilc
package feature
package booleans

trait Types extends base.Types with sums.Types with unit.Types {
  val BoolType = SumType(UnitType, UnitType)
}

trait Library {
  type ObjBool = Either[Unit, Unit]

  //Do we have any encoding of Booleans to follow?
  def boolToObjBool(b: Boolean): ObjBool =
    if (b)
      Left(())
    else
      Right(())
}

trait Syntax extends Types {
}
