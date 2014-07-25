package ilc
package language
package gcc

import ilc.language.GCC._

object TestGCC {
  val x = Var("x", IntType)                       //> x  : ilc.language.GCC.Var = Var(x,ℤ)
  val body = Var("body", UnitType) //XXX          //> body  : ilc.language.GCC.Var = Var(body,UnitType)
  val unitVar = Var("unit", UnitType)             //> unitVar  : ilc.language.GCC.Var = Var(unit,UnitType)
  val t0 = LetRecStar(List((x, 1), (body, PlusInt ! x ! x)), body)
                                                  //> t0  : ilc.language.GCC.LetRecStar = LetRecStar(List((Var(x,ℤ),LiteralInt(1
                                                  //| )), (Var(body,UnitType),App(App(PlusInt,Var(x,ℤ)),Var(x,ℤ)))),Var(body,U
                                                  //| nitType))
  toProg(t0)                                      //> res0: (List[ilc.language.GCC.Instr], scala.collection.Map[ilc.language.GCC.V
                                                  //| ar,Int]) = (List(DUM(2), LDC(1), LD(DeBrujinIdx(0,0,Var(x,ℤ))), LD(DeBruji
                                                  //| nIdx(0,0,Var(x,ℤ))), ADD, LD(DeBrujinIdx(0,1,Var(body,UnitType))), RAP(2),
                                                  //|  RTN),Map())
  val t = LetRecStar(List((x, 1), (body, Abs(unitVar, PlusInt ! x ! x))), body)
                                                  //> t  : ilc.language.GCC.LetRecStar = LetRecStar(List((Var(x,ℤ),LiteralInt(1)
                                                  //| ), (Var(body,UnitType),Abs(Var(unit,UnitType),App(App(PlusInt,Var(x,ℤ)),Va
                                                  //| r(x,ℤ))))),Var(body,UnitType))
  pretty(t)                                       //> res1: String = LetRecStar(List((Var(x,ℤ),LiteralInt(1)), (Var(body,UnitTyp
                                                  //| e),Abs(Var(unit,UnitType),App(App(PlusInt,Var(x,ℤ)),Var(x,ℤ))))),Var(bod
                                                  //| y,UnitType))

  toProcBase(t)                                   //> res2: List[ilc.language.GCC.Instr] = List(DUM(2), LDC(1), LDF(Var(fun_1,Unit
                                                  //| Type → ℤ)), LD(DeBrujinIdx(0,1,Var(body,UnitType))), RAP(2), RTN)
  toProg(t)                                       //> res3: (List[ilc.language.GCC.Instr], scala.collection.Map[ilc.language.GCC.V
                                                  //| ar,Int]) = (List(DUM(2), LDC(1), LDF(Var(fun_2,UnitType → ℤ)), LD(DeBruj
                                                  //| inIdx(0,1,Var(body,UnitType))), RAP(2), RTN, LD(DeBrujinIdx(1,0,Var(x,ℤ)))
                                                  //| , LD(DeBrujinIdx(1,0,Var(x,ℤ))), ADD, RTN),Map(Var(fun_2,UnitType → ℤ)
                                                  //|  -> 6))
}