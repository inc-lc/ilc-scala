package ilc
package feature
package maps

import org.scalatest.FunSuite

class MapsPrettySuite
extends FunSuite
   with maps.Syntax
   with functions.Pretty
{
  // dummy types for testing
  case object A extends Type
  case object B extends Type
  case object ATerm extends Term { lazy val getType = A }

  test("may give type arguments to constants") {
    info(pretty(EmptyMap.tapply(A, B)))
  }

  test("need not give argument type of polymorphic lookup") {
    val t = Lookup ! ATerm ! EmptyMap.tapply(A, B)
    info(pretty(t))
  }
}
