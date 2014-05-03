package longRunning.feature.abelianMaps

import org.scalatest.FunSuite
import org.scalatest.Matchers
import ilc.util.EvalGenerated
import ilc.feature._
import ilc.feature.abelianGroups
import ilc.feature.abelianMaps.AbelianDerivation
import ilc.feature.abelianMaps.Library._
import ilc.feature.abelianMaps.MapChanges
import ilc.feature.abelianMaps.ToScala

class DeltaAbelianMapSuite
extends FunSuite
   with Matchers
   with AbelianDerivation
   with integers.AbelianDerivation
   with MapChanges
   with functions.SyntaxSugar
   with integers.SyntaxSugar
   with maps.SyntaxSugar
   with ToScala
   with booleans.ToScala
   with integers.ToScala
   with functions.ToScala
   with products.ToScala
   with sums.ToScala
   with EvalGenerated
{
  val additiveIntegerGroup = evalGenerated(additiveGroupOnIntegers).
    asInstanceOf[abelianGroups.Library.AbelianGroup[Int]]

  val updateInput = evalGenerated(updateTerm(MapType(IntType, IntType))).
    asInstanceOf[(=>Any) => (=>Any) => Any]

  def expectToGetFrom
    (input: => AbelianMap[Int, Int])
    (oldResultCode: => Term)
    (programBody: Name => TermBuilder)
  {
    val programCode: Term = lambda(AbelianGroupType(ℤ))(programBody)
    val program = evalGenerated(programCode).
      asInstanceOf[(=>Any) => (=>Any) => Any](additiveIntegerGroup)
    val derivative = evalGenerated(derive(programCode)).
      asInstanceOf[(=>Any) => (=>Any) => (=>Any) => (=>Any) => Any](
        additiveIntegerGroup)(additiveIntegerGroup)
    val updateOutput = evalGenerated(updateTerm(oldResultCode.getType)).
      asInstanceOf[(=>Any) => (=>Any) => Any]
    val oldResult = evalGenerated(oldResultCode)
    program(input) should be(oldResult)
    for (change <- getChanges(input)) {
      updateOutput(derivative(input)(change))(oldResult) should
        be(program(updateInput(change)(input)))
    }
  }

  def getChanges(input: AbelianMap[Int, Int]):
      Iterable[ChangeToMaps[Int, Int]] =
    changesToMapsBetweenIntegers map { case (key, mkChange) =>
      mkChange(additiveIntegerGroup)(input)
    }

  test("sum of values") {
    expectToGetFrom(AbelianMap((1 to 100) map {i => (i, i)}: _*)) {
      LiteralInt(5050)
    } { _Gi =>
      FoldByHom ! _Gi ! _Gi ! snd%(ℤ, ℤ)
    }
  }
}
