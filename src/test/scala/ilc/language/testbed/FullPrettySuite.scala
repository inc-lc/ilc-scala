package ilc
package language
package testbed

import feature._

import org.scalatest.FunSuite

class FullPrettySuite
extends FunSuite

   with bintrees.Pretty  // nullary
   with integers.Pretty  // nullary
   with lists.Pretty     // nullary
   with naturals.Pretty  // nullary
   with unit.Pretty      // nullary

   with functions.Pretty // binary
   with sums.Pretty      // binary
   with products.Pretty  // binary
{
  // dummy types
  case object A extends Type
  case object B extends Type
  case object C extends Type
  case object D extends Type

  val fun2 = (A =>: B) =>: (C =>: D)
  val sum2 = SumType(SumType(A, B), SumType(C, D))
  val pro2 = ProductType(ProductType(A, B), ProductType(C, D))

  val funSum = A =>: SumType(B, C)
  val funPro = A =>: ProductType(B, C)
  val sumFun = SumType(A, B =>: C)
  val sumPro = SumType(A, ProductType(B, C))
  val proFun = ProductType(A, B =>: C)
  val proSum = ProductType(A, SumType(B, C))

  test("Should pretty-print nullary mixfix operators") {
    assert(pretty(BinTreeType(BinTreeType(A))) == "<# <# A #> #>")
    assert(pretty(ListType(BinTreeType(A)))    == "[<# A #>]")
    assert(pretty(BinTreeType(ListType(A)))    == "<# [A] #>")
    assert(pretty(ListType(ListType(A)))       == "[[A]]")

    assert(pretty(IntType)  == "Z")
    assert(pretty(NatType)  == "N")
    assert(pretty(UnitType) == "1")
  }

  test("Pretty printer should respect associativity") {
    assert(pretty(fun2) == "(A -> B) -> C -> D")
    assert(pretty(sum2) == "A + B + (C + D)")
    assert(pretty(pro2) == "A * B * (C * D)")
  }

  test("Pretty printer should respect precedence") {
    assert(pretty(funSum) == "A -> B + C")
    assert(pretty(funPro) == "A -> B * C")
    assert(pretty(sumFun) == "A + (B -> C)")
    assert(pretty(sumPro) == "A + B * C")
    assert(pretty(proFun) == "A * (B -> C)")
    assert(pretty(proSum) == "A * (B + C)")
  }
}
