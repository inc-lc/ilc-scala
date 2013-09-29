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

trait SyntaxSugar extends Types with functions.SyntaxSugar with sums.SyntaxSugar with unit.Syntax {
  val trueTerm = Inj1(UnitType) ! UnitTerm ofType BoolType
  val falseTerm = Inj2(UnitType) ! UnitTerm ofType BoolType

  def ifTerm(cond: TermBuilder, thenBranch: TermBuilder, elseBranch: TermBuilder) =
    case2(cond,
      //const ! thenBranch % UnitType,
      //const ! elseBranch % UnitType)
      lambda(Var("ignored", UnitType)) {ignored => thenBranch},
      lambda(Var("ignored", UnitType)) {ignored => elseBranch})
}
