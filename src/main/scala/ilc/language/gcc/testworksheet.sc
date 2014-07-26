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
    typecheck {
      letrec(
      	fun('go)('n) { 'to('n + 1) },
        fun('to)('n) { 'go('n - 1) },
        fun('foo)('n) {
        	if_('n === 5) {
        		'n
        	} else_ {
        		43
        	}
        })("main", 'go(42))
    }                                             //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(14,
                                                  //| Some(UApp(UVar(go),UApp(UApp(UMonomorphicConstant(Minus),UVar(n)),UMonomorph
                                                  //| icConstant(LiteralInt(1))))))),Abs(Var(n,ℤ),App(Var(to,ℤ → TypeVariabl
                                                  //| e(14,Some(UApp(UVar(go),UApp(UApp(UMonomorphicConstant(Minus),UVar(n)),UMono
                                                  //| morphicConstant(LiteralInt(1))))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1)))
                                                  //| )), (Var(to,ℤ → TypeVariable(14,Some(UApp(UVar(go),UApp(UApp(UMonomorphi
                                                  //| cConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),Abs(Var(n
                                                  //| ,ℤ),App(Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),UApp(UApp(UMonom
                                                  //| orphicConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),App(
                                                  //| App(Minus,Var(n,ℤ)),LiteralInt(1))))), (Var(foo,ℤ → ℤ),Abs(Var(n,ℤ
                                                  //| ),App(App(App(IfThenElse(ℤ),App(App(Eq,Var(n,ℤ)),LiteralInt(5))),Abs(Var
                                                  //| (_lit,UnitType),Var(n,ℤ))),Abs(Var(_lit,UnitType),LiteralInt(43)))))),main
                                                  //| ,App(Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),UApp(UApp(UMonomorphi
                                                  //| cConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),LiteralIn
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
                                                  //| 10: LD 1 1		; var Var(to,ℤ → TypeVariable(14,Some(UApp(UVar(go),
                                                  //| UApp(UApp(UMonomorphicConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralI
                                                  //| nt(1)))))))
                                                  //| 11: AP 1
                                                  //| 12: RTN
                                                  //| 13: LD 0 0		; var Var(n,ℤ)
                                                  //| 14: LDC 1
                                                  //| 15: SUB
                                                  //| 16: LD 1 0		; var Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),
                                                  //| UApp(UApp(UMonomorphicConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralI
                                                  //| nt(1)))))))
                                                  //| 17: AP 1
                                                  //| 18: RTN
                                                  //| 19: LD 0 0		; var Var(n,ℤ)
                                                  //| 20: JOIN
                                                  //| 21: LDC 43
                                                  //| 22: JOIN
                                                  //| 23: LD 0 0		; var Var(n,ℤ)
                                                  //| 24: LDC 5
                                                  //| 25: CEQ
                                                  //| 26: SEL if_t_1 if_f_2
                                                  //| 27: RTN
                                                  //| 28: LDC 42
                                                  //| 29: LD 0 0		; var Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),
                                                  //| UApp(UApp(UMonomorphicConstant(Minus),UVar(n)),UMonomorphicConstant(LiteralI
                                                  //| nt(1)))))))
                                                  //| 30: AP 1
                                                  //| 31: RTN
                                                  //| 
                                                  //| 28: Var(main,UnitType)
                                                  //| 21: Var(if_f_2,UnitType → ℤ)
                                                  //| 23: Var(foo,ℤ → ℤ)
                                                  //| 19: Var(if_t_1,UnitType → ℤ)
                                                  //| 13: Var(to,ℤ → TypeVariable(14,Some(UApp(UVar(go),UApp(UApp(UMonomorphic
                                                  //| Constant(Minus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))))
                                                  //| 7: Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),UApp(UApp(UMonomorphicC
                                                  //| onstant(Minus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))))
                                                  //| [DUM 3,
                                                  //| LDF 7,
                                                  //| LDF 13,
                                                  //| LDF 23,
                                                  //| LDF 28,
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
                                                  //| JOIN,
                                                  //| LDC 43,
                                                  //| JOIN,
                                                  //| LD 0 0,
                                                  //| LDC 5,
                                                  //| CEQ,
                                                  //| SEL 19 21,
                                                  //| RTN,
                                                  //| LDC 42,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"
   
}