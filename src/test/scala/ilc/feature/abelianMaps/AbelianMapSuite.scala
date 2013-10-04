package ilc
package feature
package abelianMaps

import scala.language.implicitConversions
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import ilc.feature.abelianMaps.Library._
import ilc.util.EvalGenerated

class AbelianMapSuite
extends FunSuite
   with ShouldMatchers
   with EvalGenerated
   with Syntax
   with ToScala
   with maps.SyntaxSugar // for map building method "fromAssoc"
   with abelianGroups.Syntax
   with abelianGroups.ToScala
   with functions.SyntaxSugar
   with functions.ToScala
   with integers.Syntax
   with integers.ToScala
   with naturals.Syntax
   with naturals.ToScala
   with maybe.ToScala
{
  val ℤ = IntType

  // this is intToTerm actually
  override implicit def natToTerm(i: Int): Term = LiteralInt(i)

  val empty = EmptyMap(ℤ, ℤ)
  val singleton = SingletonMap ! 3 ! 5
  val _G_+ = AbelianGroup ! PlusInt ! NegateInt ! 0
  val sumValues = FoldByHom ! _G_+ ! _G_+ ! snd%(ℤ, ℤ)

  val getSize = FoldByHom ! _G_+ ! _G_+ ! (const ! (const ! 1)%ℤ)%ℤ

  val neg100 = fromAssoc(((1 to 100), (-100 to -1)).zipped.toSeq)

  val ones: Term = {
    val n = 10
    fromAssoc((1 to n) map {i =>
      (i, fromAssoc((1 to i) map {j => (j, 1)}))
    })
  }

  test("EmptyMap, SingletonMap, Delete, Lookup") {
    evalGenerated(empty) should be(AbelianMap.empty)

    evalGenerated(singleton) should be(AbelianMap(3 -> 5))

    evalGenerated(Delete ! 5 ! empty) should be(AbelianMap.empty)

    evalGenerated(Delete ! 5 ! singleton) should be(AbelianMap(3 -> 5))

    evalGenerated(Delete ! 3 ! singleton) should be(AbelianMap.empty)

    evalGenerated(Lookup ! 3 ! singleton) should be(Some(5))

    evalGenerated(Lookup ! 5 ! singleton) should be(None)
  }

  test("FoldByHom") {
    evalGenerated(  getSize ! neg100) should be(100)
    evalGenerated(sumValues ! neg100) should be(-5050)
  }

  test("LiftValueGroup") {
    val t: Term =
      FoldByHom ! (LiftGroup(ℤ) ! _G_+) ! (LiftGroup(ℤ) ! _G_+) !
        lambda(ℤ, MapType(ℤ, ℤ)) {
          case Seq(key, innerMap) =>
            SingletonMap ! key ! (sumValues ! innerMap)
        } ! ones
    evalGenerated(sumValues ! t) should be(55)
  }
}
