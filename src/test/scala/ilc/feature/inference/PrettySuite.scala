package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class PrettySuite
extends FlatSpec
   with Matchers
   with Pretty
   with ilc.feature.maps.Syntax
   with Integers
{
  "Pair syntax" should "give untyped abstractions" in {
    val id: UntypedTerm = 'x -> 'x
    val id2 = UAbs(UVar("x"), UVar("x"))
    assert(id === id2)
  }

  "Application" should "work" in {
    val id: UntypedTerm = 'x -> 'x
    assert(UApp(id, id) === id(id))
  }

  "Both" should "work together" in {
    assert(UApp(UAbs(UVar("x"), UVar("x")), UAbs(UVar("x"), UVar("x"))) === ('x -> 'x)('x -> 'x))
  }
}
