package ilc.language
package gcc

import GCC._
import Predef.{any2stringadd => _, _}

object testworksheet {
  ADD.show()                                      //> res0: String = ADD
  val localMod = typecheck { letrec('x -> 1)("body", 'x + 'x) }
                                                  //> localMod  : ilc.language.GCC.Term = LetRec(List((Var(x,ℤ),LiteralInt(1))),
                                                  //| body,App(App(Plus,Var(x,ℤ)),Var(x,ℤ)))
  //LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
  pretty(localMod)                                //> res1: String = LetRec(List((Var(x,ℤ),LiteralInt(1))),body,App(App(Plus,Var
                                                  //| (x,ℤ)),Var(x,ℤ)))
 
  toProcBase(localMod)                            //> res2: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(Left(Var(body,
                                                  //| UnitType))), RAP(1), RTN)
  // showProg(localMod)

  /*
  Orig:
  val goto =
    asTerm(
      letrec(
        'go -> ('n ->: 'to('n + 1)),
        'to -> ('n ->: 'go('n - 1))
        )("main", 'go(1)))
  */
  
  val goto =
    typecheck {
      letrec(
      	fun('go)('n) { 'to('n + 1) },
        fun('to)('n) { 'go('n - 1) },
        fun('foo)('n) {
        	if_('n =!= 5) {
        	  tuple('n, 43, 42).at(2, 3)
        	} else_ {
        		43
        	}
        })("main", 'go(42))
    }                                             //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(39,
                                                  //| Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))),Abs(Var(n,ℤ),A
                                                  //| pp(Var(to,ℤ → TypeVariable(39,Some(UApp(UVar(go),UMonomorphicConstant(Li
                                                  //| teralInt(42)))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1))))), (Var(to,ℤ �
                                                  //| � TypeVariable(39,Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))
                                                  //| ),Abs(Var(n,ℤ),App(Var(go,ℤ → TypeVariable(39,Some(UApp(UVar(go),UMono
                                                  //| morphicConstant(LiteralInt(42)))))),App(App(Minus,Var(n,ℤ)),LiteralInt(1))
                                                  //| ))), (Var(foo,ℤ → ℤ),Abs(Var(n,ℤ),App(App(App(IfThenElse(ℤ),App(No
                                                  //| t,App(App(Eq,Var(n,ℤ)),LiteralInt(5)))),Abs(Var(_lit,UnitType),App(Proj2(�
                                                  //| ��, ℤ),App(Proj2(ℤ, ProductType(ℤ,ℤ)),App(App(Pair(ℤ, ProductType(
                                                  //| ℤ,ℤ)),Var(n,ℤ)),App(App(Pair(ℤ, ℤ),LiteralInt(43)),LiteralInt(42))
                                                  //| ))))),Abs(Var(_lit,UnitType),LiteralInt(43)))))),main,App(Var(go,ℤ → Typ
                                                  //| eVariable(39,Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))),Lit
                                                  //| eralInt(42)))

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
                                                  //| 10: LD 1 1		; var Var(to,ℤ → TypeVariable(39,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 11: AP 1
                                                  //| 12: RTN
                                                  //| 13: LD 0 0		; var Var(n,ℤ)
                                                  //| 14: LDC 1
                                                  //| 15: SUB
                                                  //| 16: LD 1 0		; var Var(go,ℤ → TypeVariable(39,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(42))))))
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
                                                  //| 29: LDC 1
                                                  //| 30: LD 0 0		; var Var(n,ℤ)
                                                  //| 31: LDC 5
                                                  //| 32: CEQ
                                                  //| 33: SUB
                                                  //| 34: SEL if_t_1 if_f_2
                                                  //| 35: RTN
                                                  //| 36: LDC 42
                                                  //| 37: LD 0 0		; var Var(go,ℤ → TypeVariable(39,Some(UApp(UVar(go),
                                                  //| UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 38: AP 1
                                                  //| 39: RTN
                                                  //| 
                                                  //| 36: Var(main,UnitType)
                                                  //| 27: Var(if_f_2,UnitType → ℤ)
                                                  //| 13: Var(to,ℤ → TypeVariable(39,Some(UApp(UVar(go),UMonomorphicConstant(L
                                                  //| iteralInt(42))))))
                                                  //| 29: Var(foo,ℤ → ℤ)
                                                  //| 19: Var(if_t_1,UnitType → ℤ)
                                                  //| 7: Var(go,ℤ → TypeVariable(39,Some(UApp(UVar(go),UMonomorphicConstant(Li
                                                  //| teralInt(42))))))
                                                  //| [DUM 3,
                                                  //| LDF 7,
                                                  //| LDF 13,
                                                  //| LDF 29,
                                                  //| LDF 36,
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
                                                  //| LDC 1,
                                                  //| LD 0 0,
                                                  //| LDC 5,
                                                  //| CEQ,
                                                  //| SUB,
                                                  //| SEL 19 27,
                                                  //| RTN,
                                                  //| LDC 42,
                                                  //| LD 0 0,
                                                  //| AP 1,
                                                  //| RTN]"
}