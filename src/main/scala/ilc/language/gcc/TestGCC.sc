package ilc
package language
package gcc

import ilc.language.GCC._
//Needed to avoid ambiguities when using +
import Predef.{any2stringadd => _, _}
 
object TestGCC {
  ADD.show()                                      //> res0: String = ADD
  val localMod = asTerm(letrec('x -> 1)("body", 'x + 'x))
                                                  //> localMod  : ilc.language.GCC.Term = LetRec(List((Var(x,ℤ),LiteralInt(1))),
                                                  //| body,App(App(Plus,Var(x,ℤ)),Var(x,ℤ)))
  //LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
  pretty(localMod)                                //> res1: String = LetRec(List((Var(x,ℤ),LiteralInt(1))),body,App(App(Plus,Var
                                                  //| (x,ℤ)),Var(x,ℤ)))
 
  toProcBase(localMod)                            //> res2: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(Left(Var(body,
                                                  //| UnitType))), RAP(1), RTN)
  showProg(localMod)                              //> res3: String = "
                                                  //| 0: DUM 1
                                                  //| 1: LDC 1
                                                  //| 2: LDF body
                                                  //| 3: RAP 1
                                                  //| 4: RTN
                                                  //| 5: LD 0 0		; var Var(x,ℤ)
                                                  //| 6: LD 0 0		; var Var(x,ℤ)
                                                  //| 7: ADD
                                                  //| 8: RTN
                                                  //| 
                                                  //| 5: Var(body,UnitType)
                                                  //| [DUM 1,
                                                  //| LDC 1,
                                                  //| LDF 5,
                                                  //| RAP 1,
                                                  //| RTN,
                                                  //| LD 0 0,
                                                  //| LD 0 0,
                                                  //| ADD,
                                                  //| RTN]"
  val goto =
    asTerm(
      letrec(
        'go -> ('n ->: 'to('n + 1)),
        'to -> ('n ->: 'go('n - 1))
        )("main", 'go(1)))                        //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(14,
                                                  //| Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(1)))))),Abs(Var(n,ℤ),Ap
                                                  //| p(Var(to,ℤ → TypeVariable(14,Some(UApp(UVar(go),UMonomorphicConstant(Lit
                                                  //| eralInt(1)))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1))))), (Var(to,ℤ → 
                                                  //| TypeVariable(14,Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(1)))))),A
                                                  //| bs(Var(n,ℤ),App(Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),UMonomor
                                                  //| phicConstant(LiteralInt(1)))))),App(App(Minus,Var(n,ℤ)),LiteralInt(1))))))
                                                  //| ,main,App(Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),UMonomorphicCons
                                                  //| tant(LiteralInt(1)))))),LiteralInt(1)))
  showProg(goto)                                  //> res4: String = "
                                                  //| 0: DUM 2
                                                  //| 1: LDF go
                                                  //| 2: LDF to
                                                  //| 3: LDF main
                                                  //| 4: RAP 2
                                                  //| 5: RTN
                                                  //| 6: LD 0 0		; var Var(n,ℤ)
                                                  //| 7: LDC 1
                                                  //| 8: ADD
                                                  //| 9: LD 1 1		; var Var(to,ℤ → TypeVariable(14,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(1))))))
                                                  //| 10: AP 1
                                                  //| 11: RTN
                                                  //| 12: LD 0 0		; var Var(n,ℤ)
                                                  //| 13: LDC 1
                                                  //| 14: SUB
                                                  //| 15: LD 1 0		; var Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(1))))))
                                                  //| 16: AP 1
                                                  //| 17: RTN
                                                  //| 18: LDC 1
                                                  //| 19: LD 0 0		; var Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(1))))))
                                                  //| 20: AP 1
                                                  //| 21: RTN
                                                  //| 
                                                  //| 6: Var(go,ℤ → TypeVariable(14,Some(UApp(UVar(go),UMonomorphicConstant(Li
                                                  //| teralInt(1))))))
                                                  //| 12: Var(to,ℤ → TypeVariable(14,Some(UApp(UVar(go),UMonomorphicConstant(L
                                                  //| iteralInt(1))))))
                                                  //| 18: Var(main,UnitType)
                                                  //| [DUM 2,
                                                  //| LDF 6,
                                                  //| LDF 12,
                                                  //| LDF 18,
                                                  //| RAP 2,
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
                                                  //| LDC 1,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"
}