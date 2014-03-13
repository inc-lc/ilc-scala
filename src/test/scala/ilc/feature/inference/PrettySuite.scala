package ilc.feature.inference

import org.scalatest._
import scala.language.implicitConversions

// Clashing* traits add clashing implicit conversions on symbols,
// for testing that ambiguities are caught at compile time.
trait ClashingImplicitClass {
  implicit class ImplicitClass(s: Symbol) { def apply(y: Any) = ??? }
}
trait ClashingImplicitConversion {
  implicit def implicitConversion(s: Symbol) = new ApplyClass
  class ApplyClass { def apply(y: Any) = ??? }
}

class PrettySuite
extends FlatSpec
   with Pretty
   with ilc.feature.maps.Syntax
   with ilc.feature.integers.Syntax
   with ilc.util.EvalScala
{
  "If a 3rd party implicit adding `apply` method to Symbols is in scope," ++
    " then it " should "fail to compile. Error messages:" in {
      def errorCode(traitName: String): String =
        s"""|{ import ilc.feature.inference._
            |  object X extends Pretty with $traitName { 'x('y) }
            |}""".stripMargin

      def testTrait(traitName: String, header: String) {
        val error = intercept[scala.tools.reflect.ToolBoxError] {
          evalScala(errorCode(traitName))
        }
        info(s"$header. ${error.getMessage}")
      }

      testTrait(
        "ClashingImplicitClass",
        "ERROR 1 (clashing implicit class)")

      testTrait(
        "ClashingImplicitConversion",
        "ERROR 2 (clashing implicit conversion)")

      testTrait(
        "org.scalatest.Matchers",
        "ERROR 3 (ambiguity due to Matchers of scalatest v2.0)")

      // separate error message from other output, enhance readability
      info("ERRORS OVER\n\n\n")
    }

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
