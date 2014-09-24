package ilc.language
package gcc

import GCC._
import Predef.{any2stringadd => _, _}

object testworksheet {
  ADD.show()                                      //> res0: String = ADD
  val localMod = asTerm(letrec('x -> 1)("body", 'x + 'x))
                                                  //> localMod  : ilc.language.GCC.Term = LetRec(List((Var(x,Z),LiteralInt(1))),bo
                                                  //| dy,App(App(Plus,Var(x,Z)),Var(x,Z)))
  //LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
  pretty(localMod)                                //> res1: String = LetRec(List((Var(x,Z),LiteralInt(1))),body,App(App(Plus,Var(x
                                                  //| ,Z)),Var(x,Z)))

  toProcBase(localMod)                            //> res2: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(ResolvableLabe
                                                  //| ls(List(Var(body_1,UnitType)),None)), RAP(1), RTN)
  // showProg(localMod)
  
  /*
  val program = Seq(
  	fun('go)('n) { 'to('n + 1) },
		fun('to)('n) { 'go('n - 1) },
		fun('listTest)('n) { (11 ::: 'initWorld ::: empty) get(1) },
  */
  
  
  /*
  
  For LAMBDAMAN, main is a function with two arguments:

 1. the initial state of the world, encoded as below
 2. undocumented

It returns a pair containing:

 1. the initial AI state
 2. the AI step function (closure)

The AI step function has two arguments:

 1. the current AI state
 2. the current state of the world, encoded as below

It returns a pair containing:

 1. the current AI state
 2. the move, encoded as below
  
  
  */
  val program = Seq(
  	fun('go)('tuple % ((int, int, int))) {
  		'tuple.bind('a, 'b, 'c) {
  		  'b
  		}
  	})                                        //> program  : Seq[(Symbol, ilc.language.GCC.UntypedTerm)] = List(('go,UAbs(tup
                                                  //| le,Some((Z) x ((Z) x (Z))),UApp(UAbs(bind_2,None,UApp(UAbs(a,None,UApp(UAbs
                                                  //| (b,None,UApp(UAbs(c,None,UVar(b)),UApp(UPolymorphicConstant(Proj2),UApp(UPo
                                                  //| lymorphicConstant(Proj2),UVar(bind_2))))),UApp(UPolymorphicConstant(Proj1),
                                                  //| UApp(UPolymorphicConstant(Proj2),UVar(bind_2))))),UApp(UPolymorphicConstant
                                                  //| (Proj1),UVar(bind_2)))),UVar(tuple)))))
  	
		/*fun('to)('n) { 'go('n - 1) },
		fun('listTest)('n) { (11 ::: 42 ::: empty) get(1) },
		fun('foo)('n) {
			if_('n >= 5) {
				('n, (43, 42)) at(2, 3)
			} else_ {
				43
			}
		}
  )*/
  	
  
  // ++ all
  val goto = typecheck {
  	letrec((program): _*)("main", 'go(tuple(1, 2, 3)))
  }                                               //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,(Z) x ((Z) x (Z)) -> Z)
                                                  //| ,Abs(Var(tuple,(Z) x ((Z) x (Z))),App(Abs(Var(bind_2lit,(Z) x ((Z) x (Z))),
                                                  //| App(Abs(Var(a,Z),App(Abs(Var(b,Z),App(Abs(Var(c,Z),Var(b,Z)),App(Proj2(Z, Z
                                                  //| ),App(Proj2(Z, (Z) x (Z)),Var(bind_2lit,(Z) x ((Z) x (Z))))))),App(Proj1(Z,
                                                  //|  Z),App(Proj2(Z, (Z) x (Z)),Var(bind_2lit,(Z) x ((Z) x (Z))))))),App(Proj1(
                                                  //| Z, (Z) x (Z)),Var(bind_2lit,(Z) x ((Z) x (Z)))))),Var(tuple,(Z) x ((Z) x (Z
                                                  //| ))))))),main,App(Var(go,(Z) x ((Z) x (Z)) -> Z),App(App(Pair(Z, (Z) x (Z)),
                                                  //| LiteralInt(1)),App(App(Pair(Z, Z),LiteralInt(2)),LiteralInt(3)))))
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM 1
                                                  //| 1: LDF go_3
                                                  //| 2: LDF main_8
                                                  //| 3: RAP 1
                                                  //| 4: RTN
                                                  //| 5: LD 1 0		; var Var(b,Z)
                                                  //| 6: RTN
                                                  //| 7: LD 2 0		; var Var(bind_2lit,(Z) x ((Z) x (Z)))
                                                  //| 8: CDR
                                                  //| 9: CDR
                                                  //| 10: LDF fun_7
                                                  //| 11: AP 1
                                                  //| 12: RTN
                                                  //| 13: LD 1 0		; var Var(bind_2lit,(Z) x ((Z) x (Z)))
                                                  //| 14: CDR
                                                  //| 15: CAR
                                                  //| 16: LDF fun_6
                                                  //| 17: AP 1
                                                  //| 18: RTN
                                                  //| 19: LD 0 0		; var Var(bind_2lit,(Z) x ((Z) x (Z)))
                                                  //| 20: CAR
                                                  //| 21: LDF fun_5
                                                  //| 22: AP 1
                                                  //| 23: RTN
                                                  //| 24: LD 0 0		; var Var(tuple,(Z) x ((Z) x (Z)))
                                                  //| 25: LDF fun_4
                                                  //| 26: AP 1
                                                  //| 27: RTN
                                                  //| 28: LDC 1
                                                  //| 29: LDC 2
                                                  //| 30: LDC 3
                                                  //| 31: CONS
                                                  //| 32: CONS
                                                  //| 33: LD 0 0		; var Var(go,(Z) x ((Z) x (Z)) -> Z)
                                                  //| 34: AP 1
                                                  //| 35: RTN
                                                  //| 
                                                  //| 13: Var(fun_5,Z -> Z)
                                                  //| 5: Var(fun_7,Z -> Z)
                                                  //| 7: Var(fun_6,Z -> Z)
                                                  //| 28: Var(main_8,UnitType)
                                                  //| 24: Var(go_3,(Z) x ((Z) x (Z)) -> Z)
                                                  //| 19: Var(fun_4,(Z) x ((Z) x (Z)) -> Z)
                                                  //| [DUM 1,
                                                  //| LDF 24,
                                                  //| LDF 28,
                                                  //| RAP 1,
                                                  //| RTN,
                                                  //| LD 1 0,
                                                  //| RTN,
                                                  //| LD 2 0,
                                                  //| CDR,
                                                  //| CDR,
                                                  //| LDF 5,
                                                  //| AP 1,
                                                  //| RTN,
                                                  //| LD 1 0,
                                                  //| CDR,
                                                  //| CAR,
                                                  //| LDF 7,
                                                  //| AP 1,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| CAR,
                                                  //| LDF 13,
                                                  //| AP 1,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| LDF 19,
                                                  //| AP 1,
                                                  //| RTN,
                                                  //| LDC 1,
                                                  //| LDC 2,
                                                  //| LDC 3,
                                                  //| CONS,
                                                  //| CONS,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"

}