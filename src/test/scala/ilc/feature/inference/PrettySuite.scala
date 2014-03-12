package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

class PrettySuite
extends FlatSpec
   with Pretty
   with ilc.feature.maps.Syntax
   with ilc.feature.integers.Syntax
{
  "Lambda with symbols and ->:" should "work" in {
    val id: UntypedTerm = 'x ->: 'x
    val id2 = UAbs(UVar("x"), UVar("x"))
    assert(id === id2)
  }

  it should "be right associative" in {
    assert('x ->: 'x ->: 'x === 'x ->: ('x ->: 'x))
  }

  "Application" should "work" in {
    val id: UntypedTerm = 'x ->: 'x
    assert(id(id) === UApp(id, id))
  }

  it should "work for multiple parameters" in {
    val f = 'a ->: 'b ->: 'c
    assert(f('d, 'e) === UApp(UApp(f, 'd), 'e))
  }

  it should "work for many multiple parameters (let's hope I did not use the wrong fold)" in {
    assert('f('a, 'b, 'c, 'd) === UApp(UApp(UApp(UApp('f, 'a), 'b), 'c), 'd))
  }

  "Both" should "work together" in {
    assert(('x ->: 'x)('x ->: 'x) === UApp(UAbs(UVar("x"), UVar("x")), UAbs(UVar("x"), UVar("x"))))
  }

  "Type ascription" should "be prettier" in {
    assert('x.ofType(IntType) ->: 'x === TypeAscription('x ->: 'x, IntType =>: TypeVariable(1)))
  }

  it should "be lucky with precedences so this works out" in {
    assert(('x ->: 'x ofType IntType =>: IntType) === TypeAscription('x ->: 'x, IntType =>: IntType))
  }
}
