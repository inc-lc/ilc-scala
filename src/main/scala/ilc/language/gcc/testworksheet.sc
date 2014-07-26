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
        fun('listTest)('n) { (11 ::: 42 ::: empty).tail.head },
        fun('foo)('n) {
        	if_('n >= 5) {
        		('n, (43, 42)).second.second
        	} else_ {
        		43
        	}
        })("main", 'go(42)))                      //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(11,
                                                  //| Some(UApp(UVar(to),UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphi
                                                  //| cConstant(LiteralInt(1))))))),Abs(Var(n,ℤ),App(Var(to,ℤ → TypeVariable
                                                  //| (11,Some(UApp(UVar(to),UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomo
                                                  //| rphicConstant(LiteralInt(1))))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1)))))
                                                  //| , (Var(to,ℤ → TypeVariable(11,Some(UApp(UVar(to),UApp(UApp(UMonomorphicC
                                                  //| onstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),Abs(Var(n,�
                                                  //| �),App(Var(go,ℤ → TypeVariable(11,Some(UApp(UVar(to),UApp(UApp(UMonomorp
                                                  //| hicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1))))))),App(App(
                                                  //| Minus,Var(n,ℤ)),LiteralInt(1))))), (Var(listTest,TypeVariable(16,Some(UAbs
                                                  //| (n,None,UApp(UPolymorphicConstant(Head),UApp(UPolymorphicConstant(Tail),UApp
                                                  //| (UApp(UPolymorphicConstant(Cons),UMonomorphicConstant(LiteralInt(11))),UApp(
                                                  //| UApp(UPolymorphicConstant(Cons),UMonomorphicConstant(LiteralInt(42))),UPolym
                                                  //| orphicConstant(Empty)))))))) → ℤ),Abs(Var(n,TypeVariable(16,Some(UAbs(n,
                                                  //| None,UApp(UPolymorphicConstant(Head),UApp(UPolymorphicConstant(Tail),UApp(UA
                                                  //| pp(UPolymorphicConstant(Cons),UMonomorphicConstant(LiteralInt(11))),UApp(UAp
                                                  //| p(UPolymorphicConstant(Cons),UMonomorphicConstant(LiteralInt(42))),UPolymorp
                                                  //| hicConstant(Empty))))))))),App(Head(ℤ),App(Tail(ℤ),App(App(Cons(ℤ),Lit
                                                  //| eralInt(11)),App(App(Cons(ℤ),LiteralInt(42)),Empty(ℤ))))))), (Var(foo,�
                                                  //| � → ℤ),Abs(Var(n,ℤ),App(App(App(IfThenElse(ℤ),App(App(Gte,Var(n,ℤ)
                                                  //| ),LiteralInt(5))),Abs(Var(_lit,UnitType),App(Proj2(ℤ, ℤ),App(Proj2(ℤ, 
                                                  //| ProductType(ℤ,ℤ)),App(App(Pair(ℤ, ProductType(ℤ,ℤ)),Var(n,ℤ)),Ap
                                                  //| p(App(Pair(ℤ, ℤ),LiteralInt(43)),LiteralInt(42))))))),Abs(Var(_lit,UnitT
                                                  //| ype),LiteralInt(43)))))),main,App(Var(go,ℤ → TypeVariable(11,Some(UApp(U
                                                  //| Var(to),UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(L
                                                  //| iteralInt(1))))))),LiteralInt(42)))
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM 4
                                                  //| 1: LDF go
                                                  //| 2: LDF to
                                                  //| 3: LDF listTest
                                                  //| 4: LDF foo
                                                  //| 5: LDF main
                                                  //| 6: RAP 4
                                                  //| 7: RTN
                                                  //| 8: LD 0 0		; var Var(n,ℤ)
                                                  //| 9: LDC 1
                                                  //| 10: ADD
                                                  //| 11: LD 1 1		; var Var(to,ℤ → TypeVariable(11,Some(UApp(UVar(to),
                                                  //| UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralIn
                                                  //| t(1)))))))
                                                  //| 12: AP 1
                                                  //| 13: RTN
                                                  //| 14: LD 0 0		; var Var(n,ℤ)
                                                  //| 15: LDC 1
                                                  //| 16: SUB
                                                  //| 17: LD 1 0		; var Var(go,ℤ → TypeVariable(11,Some(UApp(UVar(to),
                                                  //| UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralIn
                                                  //| t(1)))))))
                                                  //| 18: AP 1
                                                  //| 19: RTN
                                                  //| 20: LDC 11
                                                  //| 21: LDC 42
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
                                                  //| 41: SEL if_t_1 if_f_2
                                                  //| 42: RTN
                                                  //| 43: LDC 42
                                                  //| 44: LD 0 0		; var Var(go,ℤ → TypeVariable(11,Some(UApp(UVar(to),
                                                  //| UApp(UApp(UMonomorphicConstant(Plus),UVar(n)),UMonomorphicConstant(LiteralIn
                                                  //| t(1)))))))
                                                  //| 45: AP 1
                                                  //| 46: RTN
                                                  //| 
                                                  //| 20: Var(listTest,TypeVariable(16,Some(UAbs(n,None,UApp(UPolymorphicConstant(
                                                  //| Head),UApp(UPolymorphicConstant(Tail),UApp(UApp(UPolymorphicConstant(Cons),U
                                                  //| MonomorphicConstant(LiteralInt(11))),UApp(UApp(UPolymorphicConstant(Cons),UM
                                                  //| onomorphicConstant(LiteralInt(42))),UPolymorphicConstant(Empty)))))))) → �
                                                  //| ��)
                                                  //| 14: Var(to,ℤ → TypeVariable(11,Some(UApp(UVar(to),UApp(UApp(UMonomorphic
                                                  //| Constant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))))
                                                  //| 8: Var(go,ℤ → TypeVariable(11,Some(UApp(UVar(to),UApp(UApp(UMonomorphicC
                                                  //| onstant(Plus),UVar(n)),UMonomorphicConstant(LiteralInt(1)))))))
                                                  //| 43: Var(main,UnitType)
                                                  //| 36: Var(if_f_2,UnitType → ℤ)
                                                  //| 38: Var(foo,ℤ → ℤ)
                                                  //| 28: Var(if_t_1,UnitType → ℤ)
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
                                                  //| LDC 42,
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