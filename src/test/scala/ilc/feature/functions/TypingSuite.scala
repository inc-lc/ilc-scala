package ilc
package feature
package functions

import org.scalatest.FunSuite

class TypingSuite
extends FunSuite
   with TypedSyntax
   with Typing
   with unit.Syntax
   with unit.Typing
   with naturals.Syntax
   with naturals.Typing
{
  def deriveConst(c: Constant) = ???

  def * = UnitType

  test("can type well-typed terms") {
    assert(typeOf(UnitTerm) === *)
    assert(typeOf("x", Map(Var("x") -> (* =>: *))) === (* =>: *))
    assert(typeOf(Var("f")("x"),
      Map(Var("f") -> (* =>: *),Var("x") -> *)) === *)
    assert(typeOf(("x" :: *) ->: "x") === (* =>: *))
  }

  test("detects ill-typed terms") {
    info(intercept[UntypedVarError] {
      typeOf(Var("x")("y"))
    }.getMessage)
    info(intercept[UntypedAbsError] {
      typeOf(("x" ->: "x")(UnitTerm))
    }.getMessage)
    info(intercept[BadAppError] {
      typeOf((("x" :: *) ->: "x")(5))
    }.getMessage)
  }

  case class Multiply(m: Term, n: Term) extends Term
  test("disallows language constructors other than Abs and App") {
    info("throws " ++ intercept[Throwable] {
      typeOf(Lambda("x" :: NatType, "y" :: NatType) ->: Multiply("x", "y"))
    }.getClass.getName)
  }
}
