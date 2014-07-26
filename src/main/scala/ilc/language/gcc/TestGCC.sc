package ilc
package language
package gcc

import ilc.language.GCC._

object TestGCC {
  val x = Var("x", IntType)                       //> x  : ilc.language.GCC.Var = Var(x,ℤ)
  val body = Var("body", UnitType) //XXX          //> body  : ilc.language.GCC.Var = Var(body,UnitType)
  val unitVar = Var("unit", UnitType)             //> unitVar  : ilc.language.GCC.Var = Var(unit,UnitType)
  //val t0 =
  //toProg(t0)
  val localMod = LetRecStar(List((x, 1)), "body", PlusInt ! x ! x)
                                                  //> localMod  : ilc.language.GCC.LetRecStar = LetRecStar(List((Var(x,ℤ),Litera
                                                  //| lInt(1))),body,App(App(PlusInt,Var(x,ℤ)),Var(x,ℤ)))
  //LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
  pretty(localMod)                                //> res0: String = LetRecStar(List((Var(x,ℤ),LiteralInt(1))),body,App(App(Plus
                                                  //| Int,Var(x,ℤ)),Var(x,ℤ)))
 
  toProcBase(localMod)                            //> res1: List[ilc.language.GCC.Instr] = List(DUM(1), LDC(1), LDF(Var(body,UnitT
                                                  //| ype)), RAP(1), RTN)
  showProg(localMod)                              //> res2: String = "
                                                  //| 0: DUM(1)
                                                  //| 1: LDC(1)
                                                  //| 2: LDF(Var(body,UnitType))
                                                  //| 3: RAP(1)
                                                  //| 4: RTN
                                                  //| 5: LD(DeBrujinIdx(0,0,Var(x,ℤ)))
                                                  //| 6: LD(DeBrujinIdx(0,0,Var(x,ℤ)))
                                                  //| 7: ADD
                                                  //| 8: RTN
                                                  //| 
                                                  //| 5: Var(body,UnitType)"
  val go = Var("go", IntType =>: UnitType)        //> go  : ilc.language.GCC.Var = Var(go,ℤ → UnitType)
  val to = Var("to", IntType =>: UnitType)        //> to  : ilc.language.GCC.Var = Var(to,ℤ → UnitType)
  val n = Var("n", IntType)                       //> n  : ilc.language.GCC.Var = Var(n,ℤ)
  val goto = LetRecStar(
    List(
      (go, Abs(n, to ! (PlusInt ! n ! 1))),
      (to, Abs(n, go ! (PlusInt ! n ! (-1))))
      ),
      "main", go ! 1)                             //> goto  : ilc.language.GCC.LetRecStar = LetRecStar(List((Var(go,ℤ → UnitTy
                                                  //| pe),Abs(Var(n,ℤ),App(Var(to,ℤ → UnitType),App(App(PlusInt,Var(n,ℤ)),
                                                  //| LiteralInt(1))))), (Var(to,ℤ → UnitType),Abs(Var(n,ℤ),App(Var(go,ℤ �696 �� UnitType),App(App(PlusInt,Var(n,ℤ)),LiteralInt(-1)))))),main,App(Var(go
                                                  //| ,ℤ → UnitType),LiteralInt(1)))
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM(2)
                                                  //| 1: LDF(Var(go,ℤ → UnitType))
                                                  //| 2: LDF(Var(to,ℤ → UnitType))
                                                  //| 3: LDF(Var(main,UnitType))
                                                  //| 4: RAP(2)
                                                  //| 5: RTN
                                                  //| 6: LDC(1)
                                                  //| 7: LD(DeBrujinIdx(0,0,Var(n,ℤ)))
                                                  //| 8: ADD
                                                  //| 9: LD(DeBrujinIdx(1,1,Var(to,ℤ → UnitType)))
                                                  //| 10: AP(1)
                                                  //| 11: RTN
                                                  //| 12: LDC(-1)
                                                  //| 13: LD(DeBrujinIdx(0,0,Var(n,ℤ)))
                                                  //| 14: ADD
                                                  //| 15: LD(DeBrujinIdx(1,0,Var(go,ℤ → UnitType)))
                                                  //| 16: AP(1)
                                                  //| 17: RTN
                                                  //| 18: LDC(1)
                                                  //| 19: LD(DeBrujinIdx(0,0,Var(go,ℤ → UnitType)))
                                                  //| 20: AP(1)
                                                  //| 21: RTN
                                                  //| 
                                                  //| 6: Var(go,ℤ → UnitType)
                                                  //| 12: Var(to,ℤ → UnitType)
                                                  //| 18: Var(main,UnitType)"
}