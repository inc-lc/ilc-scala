package ilc
package feature
package base

import org.scalatest.FunSuite

class PrettySuite
extends FunSuite
   with base.Syntax
   with base.Pretty
{
  case object Unit extends Type

  def prettyVar(name: String): Layout =
    pretty(Var(LiteralName(name), Unit))

  test("Variables should pretty-print to their names") {
    assert(prettyVar("x") == "x")
    assert(prettyVar("y") == "y")

    // beware: names must end in a lower-case letter
    val superLongName = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    assert(prettyVar(superLongName) == superLongName)
  }
}
