package ilc
package language
package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers

import Predef.{any2stringadd => _, _}
import scala.language.reflectiveCalls

class GCCPrimitivesSuite extends FunSuite with Matchers {
  import GCC._

  test("Booleans") {

    typecheck {
      lam('a % bool, 'b % bool) {
        ('a or 'b) and 'a
      }
    }

    typecheck {
      switch(42)(
          44 ==> 4,
          43 ==> 5
      ) withDefault 0
    }


  }


  test("Tuples") {
    typecheck {
     lam('tuple % ((int, int, int))) {
        'tuple.bind('a, 'b, 'c) {
          'b
        }
      }
    }
  }

  test("Assignments") {
    val prog = lam('a % bool, 'b % int) {
      { 'a <~ true  } ~:
      { debug('a)   } ~:
      { 'b <~ 42 } ~:
      { debug('b)   }
    }

    typecheck { prog }
  }

  test("Classes") {

    val prog = letrec(
      class_('Point)('x % int, 'y % int)(
        fun('move)('a % int) {
          { 'x <~ 'x + 'a }
        },
        fun('getX)('_) { 'x }
      ),

      fun('buildPoints)('_) {
        letS(
          'p := asUntyped('new_Point)(1, 4)
        )('p.call('Point, 'move)(42) ~: 'p)
      }
    )("prog", asUntyped('buildPoints)(0).call('Point, 'getX)(0))

    val progSimple  = letS(
      class_('Point)('x % int, 'y % int)(
        fun('move)('a % int) {
          { 'x <~ 'x + 'a }
        },
        fun('getX)('_) { 'x }
      ))(debug(asUntyped('new_Point)(1, 4).call('Point, 'getX)(0)))

    typecheck { prog }

  }

  test("Should build bin trees as tuples") {
    val prog = tree(leaf(4), 42, tree(leaf(4), 8, leaf(6)))

    // should be the same code as
    val prog2: UT = tuple(tuple(0, 4, 0), 42, tuple(tuple(0, 4, 0), 8, tuple(0, 6, 0)))

    toProg(prog).code shouldEqual toProg(prog2).code

  }

}
