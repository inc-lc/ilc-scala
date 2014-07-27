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

  toProcBase(localMod)                            //> res2: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(Left(Var(body_
                                                  //| 1,UnitType))), RAP(1), RTN)
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
  	fun('go)('world, 'ghosts) {
  		  		
  		(42, lam('_state, 'world) {
  			('_state, move.left)
  		})
  	})                                        //> program  : Seq[(Symbol, ilc.language.GCC.UntypedTerm)] = List(('go,UAbs(wor
                                                  //| ld,None,UAbs(ghosts,None,UApp(UApp(UPolymorphicConstant(Pair),UMonomorphicC
                                                  //| onstant(LiteralInt(42))),UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymo
                                                  //| rphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))))
                                                  //| ))
  	
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
  	letrec((program): _*)("main", 'go(42))
  }                                               //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,Z -> TypeVariable(10,So
                                                  //| me(UAbs(ghosts,None,UApp(UApp(UPolymorphicConstant(Pair),UMonomorphicConsta
                                                  //| nt(LiteralInt(42))),UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphic
                                                  //| Constant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))))) -> 
                                                  //| ProductType(Z,TypeVariable(14,Some(UAbs(_state,None,UAbs(world,None,UApp(UA
                                                  //| pp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt
                                                  //| (3))))))) -> TypeVariable(15,Some(UAbs(world,None,UApp(UApp(UPolymorphicCon
                                                  //| stant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3)))))) -> Produc
                                                  //| tType(TypeVariable(14,Some(UAbs(_state,None,UAbs(world,None,UApp(UApp(UPoly
                                                  //| morphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))
                                                  //| ),Z))),Abs(Var(world,Z),Abs(Var(ghosts,TypeVariable(10,Some(UAbs(ghosts,Non
                                                  //| e,UApp(UApp(UPolymorphicConstant(Pair),UMonomorphicConstant(LiteralInt(42))
                                                  //| ),UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVa
                                                  //| r(_state)),UMonomorphicConstant(LiteralInt(3)))))))))),App(App(Pair(Z, Type
                                                  //| Variable(14,Some(UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphicCon
                                                  //| stant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))) -> TypeV
                                                  //| ariable(15,Some(UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_
                                                  //| state)),UMonomorphicConstant(LiteralInt(3)))))) -> ProductType(TypeVariable
                                                  //| (14,Some(UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pa
                                                  //| ir),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))),Z)),LiteralInt(4
                                                  //| 2)),Abs(Var(_state,TypeVariable(14,Some(UAbs(_state,None,UAbs(world,None,UA
                                                  //| pp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(Liter
                                                  //| alInt(3)))))))),Abs(Var(world,TypeVariable(15,Some(UAbs(world,None,UApp(UAp
                                                  //| p(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(
                                                  //| 3))))))),App(App(Pair(TypeVariable(14,Some(UAbs(_state,None,UAbs(world,None
                                                  //| ,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(Li
                                                  //| teralInt(3))))))), Z),Var(_state,TypeVariable(14,Some(UAbs(_state,None,UAbs
                                                  //| (world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphic
                                                  //| Constant(LiteralInt(3))))))))),LiteralInt(3))))))))),main,App(Var(go,Z -> T
                                                  //| ypeVariable(10,Some(UAbs(ghosts,None,UApp(UApp(UPolymorphicConstant(Pair),U
                                                  //| MonomorphicConstant(LiteralInt(42))),UAbs(_state,None,UAbs(world,None,UApp(
                                                  //| UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralI
                                                  //| nt(3))))))))) -> ProductType(Z,TypeVariable(14,Some(UAbs(_state,None,UAbs(w
                                                  //| orld,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicCo
                                                  //| nstant(LiteralInt(3))))))) -> TypeVariable(15,Some(UAbs(world,None,UApp(UAp
                                                  //| p(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(
                                                  //| 3)))))) -> ProductType(TypeVariable(14,Some(UAbs(_state,None,UAbs(world,Non
                                                  //| e,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(L
                                                  //| iteralInt(3))))))),Z))),LiteralInt(42)))
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM 1
                                                  //| 1: LDF go_2
                                                  //| 2: LDF main_4
                                                  //| 3: RAP 1
                                                  //| 4: RTN
                                                  //| 5: LD 0 0		; var Var(_state,TypeVariable(14,Some(UAbs(_state,None,U
                                                  //| Abs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorp
                                                  //| hicConstant(LiteralInt(3))))))))
                                                  //| 6: LDC 3
                                                  //| 7: CONS
                                                  //| 8: RTN
                                                  //| 9: LDC 42
                                                  //| 10: LDF fun_3
                                                  //| 11: CONS
                                                  //| 12: RTN
                                                  //| 13: LDC 42
                                                  //| 14: LD 0 0		; var Var(go,Z -> TypeVariable(10,Some(UAbs(ghosts,None,
                                                  //| UApp(UApp(UPolymorphicConstant(Pair),UMonomorphicConstant(LiteralInt(42))),
                                                  //| UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(
                                                  //| _state)),UMonomorphicConstant(LiteralInt(3))))))))) -> ProductType(Z,TypeVa
                                                  //| riable(14,Some(UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphicConst
                                                  //| ant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))) -> TypeVar
                                                  //| iable(15,Some(UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_st
                                                  //| ate)),UMonomorphicConstant(LiteralInt(3)))))) -> ProductType(TypeVariable(1
                                                  //| 4,Some(UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair
                                                  //| ),UVar(_state)),UMonomorphicConstant(LiteralInt(3))))))),Z)))
                                                  //| 15: AP 1
                                                  //| 16: RTN
                                                  //| 
                                                  //| 5: Var(fun_3,TypeVariable(14,Some(UAbs(_state,None,UAbs(world,None,UApp(UAp
                                                  //| p(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(
                                                  //| 3))))))) -> TypeVariable(15,Some(UAbs(world,None,UApp(UApp(UPolymorphicCons
                                                  //| tant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3)))))) -> Product
                                                  //| Type(TypeVariable(14,Some(UAbs(_state,None,UAbs(world,None,UApp(UApp(UPolym
                                                  //| orphicConstant(Pair),UVar(_state)),UMonomorphicConstant(LiteralInt(3)))))))
                                                  //| ,Z))
                                                  //| 9: Var(go_2,Z -> TypeVariable(10,Some(UAbs(ghosts,None,UApp(UApp(UPolymorph
                                                  //| icConstant(Pair),UMonomorphicConstant(LiteralInt(42))),UAbs(_state,None,UAb
                                                  //| s(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphi
                                                  //| cConstant(LiteralInt(3))))))))) -> ProductType(Z,TypeVariable(14,Some(UAbs(
                                                  //| _state,None,UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_stat
                                                  //| e)),UMonomorphicConstant(LiteralInt(3))))))) -> TypeVariable(15,Some(UAbs(w
                                                  //| orld,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMonomorphicCo
                                                  //| nstant(LiteralInt(3)))))) -> ProductType(TypeVariable(14,Some(UAbs(_state,N
                                                  //| one,UAbs(world,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(_state)),UMon
                                                  //| omorphicConstant(LiteralInt(3))))))),Z)))
                                                  //| 13: Var(main_4,UnitType)
                                                  //| [DUM 1,
                                                  //| LDF 9,
                                                  //| LDF 13,
                                                  //| RAP 1,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| LDC 3,
                                                  //| CONS,
                                                  //| RTN,
                                                  //| LDC 42,
                                                  //| LDF 5,
                                                  //| CONS,
                                                  //| RTN,
                                                  //| LDC 42,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"

}