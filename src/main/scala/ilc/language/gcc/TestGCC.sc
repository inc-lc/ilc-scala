package ilc
package language
package gcc

import ilc.language.GCC._
//Needed to avoid ambiguities when using +
import Predef.{any2stringadd => _, _}
 
object TestGCC {
  ADD.show()
  val localMod = asTerm(letrec('x -> 1)("body", 'x + 'x))
  //LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
  pretty(localMod)
 
  toProcBase(localMod)
  showProg(localMod)
  val goto =
    asTerm(
      letrec(
        'go -> ('n ->: 'to('n + 1)),
        'to -> ('n ->: 'go('n -1))
        )("main", 'go(1)))
  showProg(goto)
}