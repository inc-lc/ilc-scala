package ilc
package language
package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers

import Predef.{any2stringadd => _, _}
import scala.language.{ implicitConversions, reflectiveCalls }

class GCCPrimitivesSuite
extends FunSuite
   with Matchers
   with Evaluation
{
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

}
