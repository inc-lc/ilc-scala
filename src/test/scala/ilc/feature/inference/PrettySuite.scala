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
   with PrettySyntax
   with ilc.feature.maps.Syntax
   with ilc.feature.integers.Syntax
   with ilc.util.EvalScala
   with ilc.feature.bags.StdLib
   with ilc.feature.abelianMaps.Syntax
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
    val id2 = UAbs("x", None, UVar("x"))
    assert(id === id2)
  }

  it should "be right associative" in {
    assert('x ->: 'y ->: 'z === 'x ->: ('y ->: 'z))
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

  it should "work with locals" in {
    val a: UntypedTerm = 'a
    val b: UntypedTerm = 'b
    val c: UntypedTerm = 'c
    val d: UntypedTerm = 'd
    assert(a(b, c, d) === 'a('b, 'c, 'd))
  }

  it should "work when even more complicated" in {
    val foldGroup: UntypedTerm = FoldGroup // from bags
    val liftGroup: UntypedTerm = LiftGroup // own apply // from abelianMaps
    val singletonMap: UntypedTerm = SingletonMap // from abelianMaps

    assert(
      'f ->: 'g ->: foldGroup(liftGroup(freeAbelianGroup),
        'e ->: singletonMap('f('e), singleton('g('e))))
      ===
      UAbs("f",None,
        UAbs("g",None,
          UApp(UApp(UPolymorphicConstant(FoldGroup),
            UApp(UPolymorphicConstant(LiftGroup), // these two UPolymorphicConstants
              UPolymorphicConstant(FreeAbelianGroup))),
            UAbs("e",None,UApp(UApp(UPolymorphicConstant(SingletonMap), // seem to be null in the other branch. Why?
              UApp(UVar("f"),UVar("e"))),
              UApp(UPolymorphicConstant(Singleton),UApp(UVar("g"),UVar("e"))))))))
    )
  }

  "Both" should "work together" in {
    assert(('x ->: 'x)('x ->: 'x) === UApp(UAbs("x", None, UVar("x")), UAbs("x", None, UVar("x"))))
  }

  "Type annotations" should "be pretty" in {
    assert('x % IntType ->: 'y === UAbs("x", Some(IntType), UVar("y")))
  }

  "Type ascription" should "be lucky with precedences so this works out" in {
    assert(('x ->: 'x ofType IntType =>: IntType) === TypeAscription('x ->: 'x, IntType =>: IntType))
  }
}
