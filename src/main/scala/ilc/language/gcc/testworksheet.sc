package ilc.language
package gcc

import GCC._
import Predef.{any2stringadd => _, _}

object testworksheet {
  ADD.show()                                      //> res0: String = ADD
  val localMod = asTerm(letrec('x -> 1)("body", 'x + 'x))
                                                  //> localMod  : ilc.language.GCC.Term = LetRec(List((Var(x,ℤ),LiteralInt(1))),
                                                  //| body,App(App(Plus,Var(x,ℤ)),Var(x,ℤ)))
  //LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
  pretty(localMod)                                //> res1: String = LetRec(List((Var(x,ℤ),LiteralInt(1))),body,App(App(Plus,Var
                                                  //| (x,ℤ)),Var(x,ℤ)))

  toProcBase(localMod)                            //> res2: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(Left(Var(body_
                                                  //| 1,UnitType))), RAP(1), RTN)
  // showProg(localMod)
  
   
  val program = Seq(
  	fun('go)('n) { 'to('n + 1) },
		fun('to)('n) { 'go('n - 1) },
		fun('listTest)('n) { (11 ::: 'initWorld ::: empty) get(1) },
		fun('foo)('n) {
			if_('n >= 5) {
				('n, (43, 42)) at(2, 3)
			} else_ {
				43
			}
		}
  )                                               //> program  : Seq[(Symbol, ilc.language.GCC.UntypedTerm)] = List(('go,UAbs(n,No
                                                  //| ne,UApp(UVar(to),UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicC
                                                  //| onstant(LiteralInt(1)))))), ('to,UAbs(n,None,UApp(UVar(go),UApp(UApp(UMonomo
                                                  //| rphicConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))), ('lis
                                                  //| tTest,UAbs(n,None,UApp(UPolymorphicConstant(Head),UApp(UPolymorphicConstant(
                                                  //| Tail),UApp(UApp(UPolymorphicConstant(Cons),UMonomorphicConstant(LiteralInt(1
                                                  //| 1))),UApp(UApp(UPolymorphicConstant(Cons),UVar(initWorld)),UPolymorphicConst
                                                  //| ant(Empty))))))), ('foo,UAbs(n,None,UApp(UApp(UApp(UPolymorphicConstant(IfTh
                                                  //| enElse),UApp(UApp(UMonomorphicConstant(Gte),UVar(n)),UMonomorphicConstant(Li
                                                  //| teralInt(5)))),UAbs(_,None,UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphi
                                                  //| cConstant(Proj2),UApp(UApp(UPolymorphicConstant(Pair),UVar(n)),UApp(UApp(UPo
                                                  //| lymorphicConstant(Pair),UMonomorphicConstant(LiteralInt(43))),UMonomorphicCo
                                                  //| nstant(LiteralInt(42)))))))),UAbs(_,None,UMonomorphicConstant(LiteralInt(43)
                                                  //| ))))))
  	
  
  // ++ all
  val goto = typecheck {
  	letrec((program): _*)("main", 'go(42))
  }                                               //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(55,
                                                  //| Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))),Abs(Var(n,ℤ),A
                                                  //| pp(Var(to,ℤ → TypeVariable(55,Some(UApp(UVar(go),UMonomorphicConstant(Li
                                                  //| teralInt(42)))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1))))), (Var(to,ℤ �
                                                  //| � TypeVariable(55,Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))
                                                  //| ),Abs(Var(n,ℤ),App(Var(go,ℤ → TypeVariable(55,Some(UApp(UVar(go),UMono
                                                  //| morphicConstant(LiteralInt(42)))))),App(App(Minus,Var(n,ℤ)),LiteralInt(1))
                                                  //| ))), (Var(listTest,TypeVariable(20,Some(UAbs(n,None,UApp(UPolymorphicConstan
                                                  //| t(Head),UApp(UPolymorphicConstant(Tail),UApp(UApp(UPolymorphicConstant(Cons)
                                                  //| ,UMonomorphicConstant(LiteralInt(11))),UApp(UApp(UPolymorphicConstant(Cons),
                                                  //| UVar(initWorld)),UPolymorphicConstant(Empty)))))))) → ℤ),Abs(Var(n,TypeV
                                                  //| ariable(20,Some(UAbs(n,None,UApp(UPolymorphicConstant(Head),UApp(UPolymorphi
                                                  //| cConstant(Tail),UApp(UApp(UPolymorphicConstant(Cons),UMonomorphicConstant(Li
                                                  //| teralInt(11))),UApp(UApp(UPolymorphicConstant(Cons),UVar(initWorld)),UPolymo
                                                  //| rphicConstant(Empty))))))))),App(Head(ℤ),App(Tail(ℤ),App(App(Cons(ℤ),L
                                                  //| iteralInt(11)),App(App(Cons(ℤ),Var(initWorld,ℤ)),Empty(ℤ))))))), (Var(
                                                  //| foo,ℤ → ℤ),Abs(Var(n,ℤ),App(App(App(IfThenElse(ℤ),App(App(Gte,Var(
                                                  //| n,ℤ)),LiteralInt(5))),Abs(Var(_lit,UnitType),App(Proj2(ℤ, ℤ),App(Proj2
                                                  //| (ℤ, ProductType(ℤ,ℤ)),App(App(Pair(ℤ, ProductType(ℤ,ℤ)),Var(n,�
                                                  //| �)),App(App(Pair(ℤ, ℤ),LiteralInt(43)),LiteralInt(42))))))),Abs(Var(_lit
                                                  //| ,UnitType),LiteralInt(43)))))),main,App(Var(go,ℤ → TypeVariable(55,Some(
                                                  //| UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))),LiteralInt(42)))
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM 4
                                                  //| 1: LDF go_2
                                                  //| 2: LDF to_3
                                                  //| 3: LDF listTest_4
                                                  //| 4: LDF foo_5
                                                  //| 5: LDF main_8
                                                  //| 6: RAP 4
                                                  //| 7: RTN
                                                  //| 8: LD 0 0		; var Var(n,ℤ)
                                                  //| 9: LDC 1
                                                  //| 10: ADD
                                                  //| 11: LD 1 1		; var Var(to,ℤ → TypeVariable(55,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 12: AP 1
                                                  //| 13: RTN
                                                  //| 14: LD 0 0		; var Var(n,ℤ)
                                                  //| 15: LDC 1
                                                  //| 16: SUB
                                                  //| 17: LD 1 0		; var Var(go,ℤ → TypeVariable(55,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 18: AP 1
                                                  //| 19: RTN
                                                  //| 20: LDC 11
                                                  //| 21: LD 2 0		; var Var(initWorld,ℤ)
                                                  //| 22: LDC 0
                                                  //| 23: CONS
                                                  //| 24: CONS
                                                  //| 25: CDR
                                                  //| 26: CAR
                                                  //| 27: RTN
                                                  //| 28: LD 0 0		; var Var(n,ℤ)
                                                  //| 29: LDC 43
                                                  //| 30: LDC 42
                                                  //| 31: CONS
                                                  //| 32: CONS
                                                  //| 33: CDR
                                                  //| 34: CDR
                                                  //| 35: JOIN
                                                  //| 36: LDC 43
                                                  //| 37: JOIN
                                                  //| 38: LD 0 0		; var Var(n,ℤ)
                                                  //| 39: LDC 5
                                                  //| 40: CGTE
                                                  //| 41: SEL if_t_6 if_f_7
                                                  //| 42: RTN
                                                  //| 43: LDC 42
                                                  //| 44: LD 0 0		; var Var(go,ℤ → TypeVariable(55,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 45: AP 1
                                                  //| 46: RTN
                                                  //| 
                                                  //| 14: Var(to_3,ℤ → TypeVariable(55,Some(UApp(UVar(go),UMonomorphicConstant
                                                  //| (LiteralInt(42))))))
                                                  //| 38: Var(foo_5,ℤ → ℤ)
                                                  //| 43: Var(main_8,UnitType)
                                                  //| 8: Var(go_2,ℤ → TypeVariable(55,Some(UApp(UVar(go),UMonomorphicConstant(
                                                  //| LiteralInt(42))))))
                                                  //| 20: Var(listTest_4,TypeVariable(20,Some(UAbs(n,None,UApp(UPolymorphicConstan
                                                  //| t(Head),UApp(UPolymorphicConstant(Tail),UApp(UApp(UPolymorphicConstant(Cons)
                                                  //| ,UMonomorphicConstant(LiteralInt(11))),UApp(UApp(UPolymorphicConstant(Cons),
                                                  //| UVar(initWorld)),UPolymorphicConstant(Empty)))))))) → ℤ)
                                                  //| 28: Var(if_t_6,UnitType → ℤ)
                                                  //| 36: Var(if_f_7,UnitType → ℤ)
                                                  //| [DUM 4,
                                                  //| LDF 8,
                                                  //| LDF 14,
                                                  //| LDF 20,
                                                  //| LDF 38,
                                                  //| LDF 43,
                                                  //| RAP 4,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| LDC 1,
                                                  //| ADD,
                                                  //| LD 1 1,
                                                  //| AP 1,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| LDC 1,
                                                  //| SUB,
                                                  //| LD 1 0,
                                                  //| AP 1,
                                                  //| RTN,
                                                  //| LDC 11,
                                                  //| LD 2 0,
                                                  //| LDC 0,
                                                  //| CONS,
                                                  //| CONS,
                                                  //| CDR,
                                                  //| CAR,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| LDC 43,
                                                  //| LDC 42,
                                                  //| CONS,
                                                  //| CONS,
                                                  //| CDR,
                                                  //| CDR,
                                                  //| JOIN,
                                                  //| LDC 43,
                                                  //| JOIN,
                                                  //| LD 0 0,
                                                  //| LDC 5,
                                                  //| CGTE,
                                                  //| SEL 28 36,
                                                  //| RTN,
                                                  //| LDC 42,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"

}