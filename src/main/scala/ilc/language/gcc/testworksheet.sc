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
 
  toProcBase(localMod)                            //> res2: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(Left(Var(body,
                                                  //| UnitType))), RAP(1), RTN)
  // showProg(localMod)

  
  
  val goto =
    asTerm(
      letrec(
      	fun('go)('n) { 'to('n + 1) },
        fun('to)('n) { 'go('n - 1) },
        fun('foo)('n) {
        	if_('n >= 5) {
        		('n, (43, 42)).right.right
        	} else_ {
        		43
        	}
        })("main", 'go(42)))                      //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(10,
                                                  //| Some(UApp(UVar(to),UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphi
                                                  //| cConstant(LiteralInt(1))))))),Abs(Var(n,ℤ),App(Var(to,ℤ → TypeVariable
                                                  //| (10,Some(UApp(UVar(to),UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomo
                                                  //| rphicConstant(LiteralInt(1))))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1)))))
                                                  //| , (Var(to,ℤ → TypeVariable(10,Some(UApp(UVar(to),UApp(UApp(UMonomorphicC
                                                  //| onstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),Abs(Var(n,�
                                                  //| �),App(Var(go,ℤ → TypeVariable(10,Some(UApp(UVar(to),UApp(UApp(UMonomorp
                                                  //| hicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),App(App(
                                                  //| Minus,Var(n,ℤ)),LiteralInt(1))))), (Var(foo,ℤ → ℤ),Abs(Var(n,ℤ),Ap
                                                  //| p(App(App(IfThenElse(ℤ),App(App(Gte,Var(n,ℤ)),LiteralInt(5))),Abs(Var(_l
                                                  //| it,UnitType),App(Proj2(ℤ, ℤ),App(Proj2(ℤ, ProductType(ℤ,ℤ)),App(Ap
                                                  //| p(Pair(ℤ, ProductType(ℤ,ℤ)),Var(n,ℤ)),App(App(Pair(ℤ, ℤ),Literal
                                                  //| Int(43)),LiteralInt(42))))))),Abs(Var(_lit,UnitType),LiteralInt(43)))))),mai
                                                  //| n,App(Var(go,ℤ → TypeVariable(10,Some(UApp(UVar(to),UApp(UApp(UMonomorph
                                                  //| icConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),LiteralIn
                                                  //| t(42)))
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM 3
                                                  //| 1: LDF go
                                                  //| 2: LDF to
                                                  //| 3: LDF foo
                                                  //| 4: LDF main
                                                  //| 5: RAP 3
                                                  //| 6: RTN
                                                  //| 7: LD 0 0		; var Var(n,ℤ)
                                                  //| 8: LDC 1
                                                  //| 9: ADD
                                                  //| 10: LD 1 1		; var Var(to,ℤ → TypeVariable(10,Some(UApp(UVar(to),
                                                  //| UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralIn
                                                  //| t(1)))))))
                                                  //| 11: AP 1
                                                  //| 12: RTN
                                                  //| 13: LD 0 0		; var Var(n,ℤ)
                                                  //| 14: LDC 1
                                                  //| 15: SUB
                                                  //| 16: LD 1 0		; var Var(go,ℤ → TypeVariable(10,Some(UApp(UVar(to),
                                                  //| UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralIn
                                                  //| t(1)))))))
                                                  //| 17: AP 1
                                                  //| 18: RTN
                                                  //| 19: LD 0 0		; var Var(n,ℤ)
                                                  //| 20: LDC 43
                                                  //| 21: LDC 42
                                                  //| 22: CONS
                                                  //| 23: CONS
                                                  //| 24: CDR
                                                  //| 25: CDR
                                                  //| 26: JOIN
                                                  //| 27: LDC 43
                                                  //| 28: JOIN
                                                  //| 29: LD 0 0		; var Var(n,ℤ)
                                                  //| 30: LDC 5
                                                  //| 31: CGTE
                                                  //| 32: SEL if_t_1 if_f_2
                                                  //| 33: RTN
                                                  //| 34: LDC 42
                                                  //| 35: LD 0 0		; var Var(go,ℤ → TypeVariable(10,Some(UApp(UVar(to),
                                                  //| UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralIn
                                                  //| t(1)))))))
                                                  //| 36: AP 1
                                                  //| 37: RTN
                                                  //| 
                                                  //| 34: Var(main,UnitType)
                                                  //| 27: Var(if_f_2,UnitType → ℤ)
                                                  //| 29: Var(foo,ℤ → ℤ)
                                                  //| 19: Var(if_t_1,UnitType → ℤ)
                                                  //| 13: Var(to,ℤ → TypeVariable(10,Some(UApp(UVar(to),UApp(UApp(UMonomorphic
                                                  //| Constant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))))
                                                  //| 7: Var(go,ℤ → TypeVariable(10,Some(UApp(UVar(to),UApp(UApp(UMonomorphicC
                                                  //| onstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))))
                                                  //| [DUM 3,
                                                  //| LDF 7,
                                                  //| LDF 13,
                                                  //| LDF 29,
                                                  //| LDF 34,
                                                  //| RAP 3,
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
                                                  //| SEL 19 27,
                                                  //| RTN,
                                                  //| LDC 42,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"
   
}