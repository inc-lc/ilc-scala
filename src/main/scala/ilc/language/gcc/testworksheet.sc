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
  
  
  val program = Seq(
  	fun('go)('n) { 'to('n + 1) },
		fun('to)('n) { 'go('n - 1) },
		fun('listTest)('n) { (11 ::: 42 ::: empty) get(1) },
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
                                                  //| 1))),UApp(UApp(UPolymorphicConstant(Cons),UMonomorphicConstant(LiteralInt(42
                                                  //| ))),UPolymorphicConstant(Empty))))))), ('foo,UAbs(n,None,UApp(UApp(UApp(UPol
                                                  //| ymorphicConstant(IfThenElse),UApp(UApp(UMonomorphicConstant(Gte),UVar(n)),UM
                                                  //| onomorphicConstant(LiteralInt(5)))),UAbs(_,None,UApp(UPolymorphicConstant(Pr
                                                  //| oj2),UApp(UPolymorphicConstant(Proj2),UApp(UApp(UPolymorphicConstant(Pair),U
                                                  //| Var(n)),UApp(UApp(UPolymorphicConstant(Pair),UMonomorphicConstant(LiteralInt
                                                  //| (43))),UMonomorphicConstant(LiteralInt(42)))))))),UAbs(_,None,UMonomorphicCo
                                                  //| nstant(LiteralInt(43)))))))
  	
  
  
  val goto = typecheck {
  	letrec((program ++ all): _*)("main", 'go(42))
  }                                               //> goto  : ilc.language.GCC.Term = LetRec(List((Var(go,ℤ → TypeVariable(321
                                                  //| ,Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42)))))),Abs(Var(n,ℤ),
                                                  //| App(Var(to,ℤ → TypeVariable(321,Some(UApp(UVar(go),UMonomorphicConstant(
                                                  //| LiteralInt(42)))))),App(App(Plus,Var(n,ℤ)),LiteralInt(1))))), (Var(to,ℤ 
                                                  //| → TypeVariable(321,Some(UApp(UVar(go),UMonomorphicConstant(LiteralInt(42))
                                                  //| )))),Abs(Var(n,ℤ),App(Var(go,ℤ → TypeVariable(321,Some(UApp(UVar(go),U
                                                  //| MonomorphicConstant(LiteralInt(42)))))),App(App(Minus,Var(n,ℤ)),LiteralInt
                                                  //| (1))))), (Var(listTest,TypeVariable(43,Some(UAbs(n,None,UApp(UPolymorphicCon
                                                  //| stant(Head),UApp(UPolymorphicConstant(Tail),UApp(UApp(UPolymorphicConstant(C
                                                  //| ons),UMonomorphicConstant(LiteralInt(11))),UApp(UApp(UPolymorphicConstant(Co
                                                  //| ns),UMonomorphicConstant(LiteralInt(42))),UPolymorphicConstant(Empty))))))))
                                                  //|  → ℤ),Abs(Var(n,TypeVariable(43,Some(UAbs(n,None,UApp(UPolymorphicConsta
                                                  //| nt(Head),UApp(UPolymorphicConstant(Tail),UApp(UApp(UPolymorphicConstant(Cons
                                                  //| ),UMonomorphicConstant(LiteralInt(11))),UApp(UApp(UPolymorphicConstant(Cons)
                                                  //| ,UMonomorphicConstant(LiteralInt(42))),UPolymorphicConstant(Empty))))))))),A
                                                  //| pp(Head(ℤ),App(Tail(ℤ),App(App(Cons(ℤ),LiteralInt(11)),App(App(Cons(�
                                                  //| �),LiteralInt(42)),Empty(ℤ))))))), (Var(foo,ℤ → ℤ),Abs(Var(n,ℤ),Ap
                                                  //| p(App(App(IfThenElse(ℤ),App(App(Gte,Var(n,ℤ)),LiteralInt(5))),Abs(Var(_l
                                                  //| it,UnitType),App(Proj2(ℤ, ℤ),App(Proj2(ℤ, ProductType(ℤ,ℤ)),App(Ap
                                                  //| p(Pair(ℤ, ProductType(ℤ,ℤ)),Var(n,ℤ)),App(App(Pair(ℤ, ℤ),Literal
                                                  //| Int(43)),LiteralInt(42))))))),Abs(Var(_lit,UnitType),LiteralInt(43))))), (Va
                                                  //| r(foldRight,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(
                                                  //| Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),Typ
                                                  //| eVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstan
                                                  //| t(Proj2),UVar(el)))))))) → ListType(ProductType(TypeVariable(201,Some(UPol
                                                  //| ymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicCons
                                                  //| tant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Pr
                                                  //| oj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) 
                                                  //| → (ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Product
                                                  //| Type(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,So
                                                  //| me(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el
                                                  //| ))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(
                                                  //| Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),Pro
                                                  //| ductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → ListType(Prod
                                                  //| uctType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(Type
                                                  //| Variable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204
                                                  //| ,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar
                                                  //| (el))))),ProductType(ℤ,ℤ)))))) → ListType(ProductType(TypeVariable(201
                                                  //| ,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolym
                                                  //| orphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicC
                                                  //| onstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ
                                                  //| ,ℤ)))))),Abs(Var(list,ListType(ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))))))),Abs(Var(z,ListType(ProductType(TypeVar
                                                  //| iable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,So
                                                  //| me(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPol
                                                  //| ymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Produc
                                                  //| tType(ℤ,ℤ)))))),Abs(Var(fun,ProductType(TypeVariable(201,Some(UPolymorph
                                                  //| icConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(P
                                                  //| roj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymor
                                                  //| phicConstant(Proj2),UVar(el))))))) → ListType(ProductType(TypeVariable(201
                                                  //| ,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolym
                                                  //| orphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicC
                                                  //| onstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ
                                                  //| ,ℤ))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))),LetRec(List((V
                                                  //| ar(go,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVaria
                                                  //| ble(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj
                                                  //| 2),UVar(el)))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorph
                                                  //| icConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(P
                                                  //| roj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),U
                                                  //| App(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))),Abs(V
                                                  //| ar(l,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))
                                                  //| ),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariab
                                                  //| le(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2
                                                  //| ),UVar(el))))))))),App(App(App(IfThenElse(ListType(ProductType(TypeVariable(
                                                  //| 201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPo
                                                  //| lymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorph
                                                  //| icConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(
                                                  //| ℤ,ℤ)))))),App(IsEmpty(ProductType(TypeVariable(201,Some(UPolymorphicCons
                                                  //| tant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))
                                                  //| ),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCo
                                                  //| nstant(Proj2),UVar(el)))))))),Var(l,ListType(ProductType(TypeVariable(201,So
                                                  //| me(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorp
                                                  //| hicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),
                                                  //| UApp(UPolymorphicConstant(Proj2),UVar(el))))))))))),Abs(Var(_lit,UnitType),V
                                                  //| ar(z,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))
                                                  //| ),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductTyp
                                                  //| e(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCo
                                                  //| nstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))))),Abs(Var(_lit,UnitType
                                                  //| ),App(App(Var(fun,ProductType(TypeVariable(201,Some(UPolymorphicConstant(Pro
                                                  //| j2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVa
                                                  //| riable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(P
                                                  //| roj2),UVar(el))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant
                                                  //| (Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2)
                                                  //| ,UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → 
                                                  //| ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Pro
                                                  //| ductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(Typ
                                                  //| eVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstan
                                                  //| t(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))),App(Head(ProductType(TypeVar
                                                  //| iable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,So
                                                  //| me(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicCons
                                                  //| tant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))))),Var(l,ListType(
                                                  //| ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(
                                                  //| TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UA
                                                  //| pp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))
                                                  //| )))))),App(Var(go,ListType(ProductType(TypeVariable(201,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)
                                                  //| )),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicC
                                                  //| onstant(Proj2),UVar(el)))))))) → ListType(ProductType(TypeVariable(201,Som
                                                  //| e(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorph
                                                  //| icConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConst
                                                  //| ant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ
                                                  //| )))))),App(Tail(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2
                                                  //| ))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVari
                                                  //| able(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Pro
                                                  //| j2),UVar(el)))))))),Var(l,ListType(ProductType(TypeVariable(201,Some(UPolymo
                                                  //| rphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstan
                                                  //| t(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))))))))))))))),foldRightBody,App(Var(go,Lis
                                                  //| tType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Produc
                                                  //| tType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,S
                                                  //| ome(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(e
                                                  //| l)))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))),Var(list,ListT
                                                  //| ype(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductT
                                                  //| ype(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Som
                                                  //| e(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)
                                                  //| )))))))))))))), (Var(map,ListType(ProductType(TypeVariable(201,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant
                                                  //| (Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolym
                                                  //| orphicConstant(Proj2),UVar(el)))))))) → (ProductType(TypeVariable(201,Some
                                                  //| (UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphi
                                                  //| cConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UA
                                                  //| pp(UPolymorphicConstant(Proj2),UVar(el))))))) → ProductType(TypeVariable(2
                                                  //| 01,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPol
                                                  //| ymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphi
                                                  //| cConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(�652 ��,ℤ))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConst
                                                  //| ant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)))
                                                  //| ,ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPo
                                                  //| lymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))),Abs(Var(list
                                                  //| ,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Pr
                                                  //| oductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(2
                                                  //| 04,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UV
                                                  //| ar(el))))))))),Abs(Var(fun,ProductType(TypeVariable(201,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)
                                                  //| )),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicC
                                                  //| onstant(Proj2),UVar(el))))))) → ProductType(TypeVariable(201,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant
                                                  //| (Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2)
                                                  //| ,UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))),App(
                                                  //| App(App(Var(foldRight,ListType(ProductType(TypeVariable(201,Some(UPolymorphi
                                                  //| cConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Pr
                                                  //| oj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorp
                                                  //| hicConstant(Proj2),UVar(el)))))))) → ListType(ProductType(TypeVariable(201
                                                  //| ,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolym
                                                  //| orphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicC
                                                  //| onstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ
                                                  //| ,ℤ))))) → (ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVaria
                                                  //| ble(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj
                                                  //| 2),UVar(el))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphi
                                                  //| cConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Pr
                                                  //| oj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UA
                                                  //| pp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → Lis
                                                  //| tType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Produc
                                                  //| tType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVa
                                                  //| riable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(P
                                                  //| roj2),UVar(el))))),ProductType(ℤ,ℤ)))))) → ListType(ProductType(TypeVa
                                                  //| riable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,S
                                                  //| ome(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPo
                                                  //| lymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Produ
                                                  //| ctType(ℤ,ℤ)))))),Var(list,ListType(ProductType(TypeVariable(201,Some(UPo
                                                  //| lymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicCon
                                                  //| stant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(U
                                                  //| PolymorphicConstant(Proj2),UVar(el)))))))))),Empty(ProductType(TypeVariable(
                                                  //| 201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPo
                                                  //| lymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorph
                                                  //| icConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(
                                                  //| ℤ,ℤ)))))),Abs(Var(head,ProductType(TypeVariable(201,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)
                                                  //| )),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicC
                                                  //| onstant(Proj2),UVar(el)))))))),Abs(Var(tail,ListType(ProductType(TypeVariabl
                                                  //| e(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(U
                                                  //| PolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymor
                                                  //| phicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductTyp
                                                  //| e(ℤ,ℤ)))))),App(App(Cons(ProductType(TypeVariable(201,Some(UPolymorphicC
                                                  //| onstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj
                                                  //| 2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp
                                                  //| (UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))),App(Var(f
                                                  //| un,ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductTy
                                                  //| pe(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some
                                                  //| (UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))
                                                  //| ))))) → ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Pr
                                                  //| oductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(Ty
                                                  //| peVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConsta
                                                  //| nt(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))),Var(head,ProductType(TypeVar
                                                  //| iable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,So
                                                  //| me(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicCons
                                                  //| tant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))))))),Var(tail,List
                                                  //| Type(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Product
                                                  //| Type(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVar
                                                  //| iable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Pr
                                                  //| oj2),UVar(el))))),ProductType(ℤ,ℤ))))))))))))), (Var(world_map,ProductTy
                                                  //| pe(TypeVariable(114,Some(UApp(UPolymorphicConstant(Proj1),UVar(world)))),Typ
                                                  //| eVariable(113,Some(UPolymorphicConstant(Proj1)))) → TypeVariable(114,Some(
                                                  //| UApp(UPolymorphicConstant(Proj1),UVar(world))))),Abs(Var(world,ProductType(T
                                                  //| ypeVariable(114,Some(UApp(UPolymorphicConstant(Proj1),UVar(world)))),TypeVar
                                                  //| iable(113,Some(UPolymorphicConstant(Proj1))))),App(Proj1(TypeVariable(114,So
                                                  //| me(UApp(UPolymorphicConstant(Proj1),UVar(world)))), TypeVariable(113,Some(UP
                                                  //| olymorphicConstant(Proj1)))),Var(world,ProductType(TypeVariable(114,Some(UAp
                                                  //| p(UPolymorphicConstant(Proj1),UVar(world)))),TypeVariable(113,Some(UPolymorp
                                                  //| hicConstant(Proj1)))))))), (Var(world_lambdaStatus,ProductType(TypeVariable(
                                                  //| 118,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(121,Some(UAp
                                                  //| p(UPolymorphicConstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world)))
                                                  //| )),TypeVariable(117,Some(UPolymorphicConstant(Proj1))))) → TypeVariable(12
                                                  //| 1,Some(UApp(UPolymorphicConstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVa
                                                  //| r(world)))))),Abs(Var(world,ProductType(TypeVariable(118,Some(UPolymorphicCo
                                                  //| nstant(Proj2))),ProductType(TypeVariable(121,Some(UApp(UPolymorphicConstant(
                                                  //| Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world))))),TypeVariable(117,Som
                                                  //| e(UPolymorphicConstant(Proj1)))))),App(Proj1(TypeVariable(121,Some(UApp(UPol
                                                  //| ymorphicConstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world))))), Ty
                                                  //| peVariable(117,Some(UPolymorphicConstant(Proj1)))),App(Proj2(TypeVariable(11
                                                  //| 8,Some(UPolymorphicConstant(Proj2))), ProductType(TypeVariable(121,Some(UApp
                                                  //| (UPolymorphicConstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world))))
                                                  //| ),TypeVariable(117,Some(UPolymorphicConstant(Proj1))))),Var(world,ProductTyp
                                                  //| e(TypeVariable(118,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariab
                                                  //| le(121,Some(UApp(UPolymorphicConstant(Proj1),UApp(UPolymorphicConstant(Proj2
                                                  //| ),UVar(world))))),TypeVariable(117,Some(UPolymorphicConstant(Proj1))))))))))
                                                  //| , (Var(world_itemAt,ProductType(ListType(ListType(TypeVariable(137,Some(UPol
                                                  //| ymorphicConstant(Tail))))),TypeVariable(164,Some(UPolymorphicConstant(Proj1)
                                                  //| ))) → ℤ → ℤ → TypeVariable(137,Some(UPolymorphicConstant(Tail)))),
                                                  //| Abs(Var(world,ProductType(ListType(ListType(TypeVariable(137,Some(UPolymorph
                                                  //| icConstant(Tail))))),TypeVariable(164,Some(UPolymorphicConstant(Proj1))))),A
                                                  //| bs(Var(x,ℤ),Abs(Var(y,ℤ),LetRec(List((Var(go,ListType(TypeVariable(137,S
                                                  //| ome(UPolymorphicConstant(Tail)))) → ℤ → TypeVariable(137,Some(UPolymor
                                                  //| phicConstant(Tail)))),Abs(Var(l,ListType(TypeVariable(137,Some(UPolymorphicC
                                                  //| onstant(Tail))))),Abs(Var(i,ℤ),App(App(App(IfThenElse(TypeVariable(137,Som
                                                  //| e(UPolymorphicConstant(Tail)))),App(App(Eq,Var(i,ℤ)),LiteralInt(0))),Abs(V
                                                  //| ar(_lit,UnitType),App(Head(TypeVariable(137,Some(UPolymorphicConstant(Tail))
                                                  //| )),Var(l,ListType(TypeVariable(137,Some(UPolymorphicConstant(Tail)))))))),Ab
                                                  //| s(Var(_lit,UnitType),App(App(Var(go,ListType(TypeVariable(137,Some(UPolymorp
                                                  //| hicConstant(Tail)))) → ℤ → TypeVariable(137,Some(UPolymorphicConstant(
                                                  //| Tail)))),App(Tail(TypeVariable(137,Some(UPolymorphicConstant(Tail)))),Var(l,
                                                  //| ListType(TypeVariable(137,Some(UPolymorphicConstant(Tail))))))),App(App(Minu
                                                  //| s,Var(i,ℤ)),LiteralInt(1))))))))),elemAtBody,App(App(Var(go,ListType(TypeV
                                                  //| ariable(137,Some(UPolymorphicConstant(Tail)))) → ℤ → TypeVariable(137,
                                                  //| Some(UPolymorphicConstant(Tail)))),LetRec(List((Var(go,ListType(ListType(Typ
                                                  //| eVariable(137,Some(UPolymorphicConstant(Tail))))) → ℤ → ListType(TypeV
                                                  //| ariable(137,Some(UPolymorphicConstant(Tail))))),Abs(Var(l,ListType(ListType(
                                                  //| TypeVariable(137,Some(UPolymorphicConstant(Tail)))))),Abs(Var(i,ℤ),App(App
                                                  //| (App(IfThenElse(ListType(TypeVariable(137,Some(UPolymorphicConstant(Tail))))
                                                  //| ),App(App(Eq,Var(i,ℤ)),LiteralInt(0))),Abs(Var(_lit,UnitType),App(Head(Lis
                                                  //| tType(TypeVariable(137,Some(UPolymorphicConstant(Tail))))),Var(l,ListType(Li
                                                  //| stType(TypeVariable(137,Some(UPolymorphicConstant(Tail))))))))),Abs(Var(_lit
                                                  //| ,UnitType),App(App(Var(go,ListType(ListType(TypeVariable(137,Some(UPolymorph
                                                  //| icConstant(Tail))))) → ℤ → ListType(TypeVariable(137,Some(UPolymorphic
                                                  //| Constant(Tail))))),App(Tail(ListType(TypeVariable(137,Some(UPolymorphicConst
                                                  //| ant(Tail))))),Var(l,ListType(ListType(TypeVariable(137,Some(UPolymorphicCons
                                                  //| tant(Tail)))))))),App(App(Minus,Var(i,ℤ)),LiteralInt(1))))))))),elemAtBody
                                                  //| ,App(App(Var(go,ListType(ListType(TypeVariable(137,Some(UPolymorphicConstant
                                                  //| (Tail))))) → ℤ → ListType(TypeVariable(137,Some(UPolymorphicConstant(T
                                                  //| ail))))),App(Proj1(ListType(ListType(TypeVariable(137,Some(UPolymorphicConst
                                                  //| ant(Tail))))), TypeVariable(164,Some(UPolymorphicConstant(Proj1)))),Var(worl
                                                  //| d,ProductType(ListType(ListType(TypeVariable(137,Some(UPolymorphicConstant(T
                                                  //| ail))))),TypeVariable(164,Some(UPolymorphicConstant(Proj1))))))),Var(y,ℤ))
                                                  //| )),Var(x,ℤ))))))), (Var(world_ghostsStatus,ProductType(TypeVariable(175,So
                                                  //| me(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(173,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(ListType(ProductType(TypeVariable(201,Some(
                                                  //| UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphic
                                                  //| Constant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UAp
                                                  //| p(UPolymorphicConstant(Proj2),UVar(el)))))))),TypeVariable(172,Some(UPolymor
                                                  //| phicConstant(Proj1)))))) → ListType(ProductType(TypeVariable(201,Some(UPol
                                                  //| ymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicCons
                                                  //| tant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Pr
                                                  //| oj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| ,Abs(Var(world,ProductType(TypeVariable(175,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(173,Some(UPolymorphicConstant(Proj2))),ProductTy
                                                  //| pe(ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),
                                                  //| ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable
                                                  //| (204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),
                                                  //| UVar(el)))))))),TypeVariable(172,Some(UPolymorphicConstant(Proj1))))))),App(
                                                  //| App(Var(map,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(
                                                  //| Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),Typ
                                                  //| eVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstan
                                                  //| t(Proj2),UVar(el)))))))) → (ProductType(TypeVariable(201,Some(UPolymorphic
                                                  //| Constant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Pro
                                                  //| j2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorph
                                                  //| icConstant(Proj2),UVar(el))))))) → ProductType(TypeVariable(201,Some(UPoly
                                                  //| morphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConst
                                                  //| ant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Pro
                                                  //| j2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) �652 �� ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),
                                                  //| ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(
                                                  //| TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCons
                                                  //| tant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))),App(Proj1(ListType(Produc
                                                  //| tType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVa
                                                  //| riable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPo
                                                  //| lymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))))), T
                                                  //| ypeVariable(172,Some(UPolymorphicConstant(Proj1)))),App(Proj2(TypeVariable(1
                                                  //| 73,Some(UPolymorphicConstant(Proj2))), ProductType(ListType(ProductType(Type
                                                  //| Variable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199
                                                  //| ,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicC
                                                  //| onstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))))),TypeVariable
                                                  //| (172,Some(UPolymorphicConstant(Proj1))))),App(Proj2(TypeVariable(175,Some(UP
                                                  //| olymorphicConstant(Proj2))), ProductType(TypeVariable(173,Some(UPolymorphicC
                                                  //| onstant(Proj2))),ProductType(ListType(ProductType(TypeVariable(201,Some(UPol
                                                  //| ymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicCons
                                                  //| tant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UP
                                                  //| olymorphicConstant(Proj2),UVar(el)))))))),TypeVariable(172,Some(UPolymorphic
                                                  //| Constant(Proj1)))))),Var(world,ProductType(TypeVariable(175,Some(UPolymorphi
                                                  //| cConstant(Proj2))),ProductType(TypeVariable(173,Some(UPolymorphicConstant(Pr
                                                  //| oj2))),ProductType(ListType(ProductType(TypeVariable(201,Some(UPolymorphicCo
                                                  //| nstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2
                                                  //| ))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphic
                                                  //| Constant(Proj2),UVar(el)))))))),TypeVariable(172,Some(UPolymorphicConstant(P
                                                  //| roj1))))))))))),Abs(Var(el,ProductType(TypeVariable(201,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)
                                                  //| )),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicC
                                                  //| onstant(Proj2),UVar(el)))))))),App(App(Pair(TypeVariable(201,Some(UPolymorph
                                                  //| icConstant(Proj2))), ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),
                                                  //| UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))),App(Pr
                                                  //| oj1(TypeVariable(201,Some(UPolymorphicConstant(Proj2))), ProductType(TypeVar
                                                  //| iable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPol
                                                  //| ymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))))),Var(
                                                  //| el,ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductTy
                                                  //| pe(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some
                                                  //| (UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))
                                                  //| )))))))),App(App(Pair(TypeVariable(199,Some(UPolymorphicConstant(Proj2))), P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))),App(Proj1(TypeVar
                                                  //| iable(199,Some(UPolymorphicConstant(Proj2))), TypeVariable(204,Some(UApp(UPo
                                                  //| lymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))),App(
                                                  //| Proj2(TypeVariable(201,Some(UPolymorphicConstant(Proj2))), ProductType(TypeV
                                                  //| ariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UP
                                                  //| olymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))))),Va
                                                  //| r(el,ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Product
                                                  //| Type(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,So
                                                  //| me(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el
                                                  //| ))))))))))),App(App(Pair(TypeVariable(204,Some(UApp(UPolymorphicConstant(Pro
                                                  //| j2),UApp(UPolymorphicConstant(Proj2),UVar(el))))), ProductType(ℤ,ℤ)),App
                                                  //| (Proj2(TypeVariable(199,Some(UPolymorphicConstant(Proj2))), TypeVariable(204
                                                  //| ,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar
                                                  //| (el)))))),App(Proj2(TypeVariable(201,Some(UPolymorphicConstant(Proj2))), Pro
                                                  //| ductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(20
                                                  //| 4,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVa
                                                  //| r(el))))))),Var(el,ProductType(TypeVariable(201,Some(UPolymorphicConstant(Pr
                                                  //| oj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeV
                                                  //| ariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(
                                                  //| Proj2),UVar(el))))))))))),App(App(Pair(ℤ, ℤ),LiteralInt(0)),LiteralInt(0
                                                  //| ))))))))), (Var(world_fruitStatus,ProductType(TypeVariable(219,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(217,Some(UPolymorphicConstant
                                                  //| (Proj2))),ProductType(TypeVariable(215,Some(UPolymorphicConstant(Proj2))),Ty
                                                  //| peVariable(223,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConsta
                                                  //| nt(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(world))))))))) → TypeVaria
                                                  //| ble(223,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj
                                                  //| 2),UApp(UPolymorphicConstant(Proj2),UVar(world))))))),Abs(Var(world,ProductT
                                                  //| ype(TypeVariable(219,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVari
                                                  //| able(217,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(215,Som
                                                  //| e(UPolymorphicConstant(Proj2))),TypeVariable(223,Some(UApp(UPolymorphicConst
                                                  //| ant(Proj2),UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2)
                                                  //| ,UVar(world)))))))))),App(Proj2(TypeVariable(215,Some(UPolymorphicConstant(P
                                                  //| roj2))), TypeVariable(223,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(world))))))),App(
                                                  //| Proj2(TypeVariable(217,Some(UPolymorphicConstant(Proj2))), ProductType(TypeV
                                                  //| ariable(215,Some(UPolymorphicConstant(Proj2))),TypeVariable(223,Some(UApp(UP
                                                  //| olymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphic
                                                  //| Constant(Proj2),UVar(world)))))))),App(Proj2(TypeVariable(219,Some(UPolymorp
                                                  //| hicConstant(Proj2))), ProductType(TypeVariable(217,Some(UPolymorphicConstant
                                                  //| (Proj2))),ProductType(TypeVariable(215,Some(UPolymorphicConstant(Proj2))),Ty
                                                  //| peVariable(223,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConsta
                                                  //| nt(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(world))))))))),Var(world,Pro
                                                  //| ductType(TypeVariable(219,Some(UPolymorphicConstant(Proj2))),ProductType(Typ
                                                  //| eVariable(217,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(21
                                                  //| 5,Some(UPolymorphicConstant
                                                  //| Output exceeds cutoff limit.
  showProg(goto)                                  //> res3: String = "
                                                  //| 0: DUM 31
                                                  //| 1: LDF go
                                                  //| 2: LDF to
                                                  //| 3: LDF listTest
                                                  //| 4: LDF foo
                                                  //| 5: LDF foldRight
                                                  //| 6: LDF map
                                                  //| 7: LDF world_map
                                                  //| 8: LDF world_lambdaStatus
                                                  //| 9: LDF world_itemAt
                                                  //| 10: LDF world_ghostsStatus
                                                  //| 11: LDF world_fruitStatus
                                                  //| 12: LDF isWall
                                                  //| 13: LDF isEmptyField
                                                  //| 14: LDF isPill
                                                  //| 15: LDF isPowerPill
                                                  //| 16: LDF isFruit
                                                  //| 17: LDF isLambdaStart
                                                  //| 18: LDF isGhostStart
                                                  //| 19: LDF vitality
                                                  //| 20: LDF location
                                                  //| 21: LDF direction
                                                  //| 22: LDF lives
                                                  //| 23: LDF score
                                                  //| 24: LDF isAfraid
                                                  //| 25: LDF isInvisible
                                                  //| 26: LDF powerLeft
                                                  //| 27: LDF isInPowerMode
                                                  //| 28: LDF isUp
                                                  //| 29: LDF isRight
                                                  //| 30: LDF isDown
                                                  //| 31: LDF isLeft
                                                  //| 32: LDF main
                                                  //| 33: RAP 31
                                                  //| 34: RTN
                                                  //| 35: LD 0 0		; var Var(n,ℤ)
                                                  //| 36: LDC 1
                                                  //| 37: ADD
                                                  //| 38: LD 1 1		; var Var(to,ℤ → TypeVariable(321,Some(UApp(UVar(go)
                                                  //| ,UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 39: AP 1
                                                  //| 40: RTN
                                                  //| 41: LD 0 0		; var Var(n,ℤ)
                                                  //| 42: LDC 1
                                                  //| 43: SUB
                                                  //| 44: LD 1 0		; var Var(go,ℤ → TypeVariable(321,Some(UApp(UVar(go)
                                                  //| ,UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 45: AP 1
                                                  //| 46: RTN
                                                  //| 47: LDC 11
                                                  //| 48: LDC 42
                                                  //| 49: LDC 0
                                                  //| 50: CONS
                                                  //| 51: CONS
                                                  //| 52: CDR
                                                  //| 53: CAR
                                                  //| 54: RTN
                                                  //| 55: LD 0 0		; var Var(n,ℤ)
                                                  //| 56: LDC 43
                                                  //| 57: LDC 42
                                                  //| 58: CONS
                                                  //| 59: CONS
                                                  //| 60: CDR
                                                  //| 61: CDR
                                                  //| 62: JOIN
                                                  //| 63: LDC 43
                                                  //| 64: JOIN
                                                  //| 65: LD 0 0		; var Var(n,ℤ)
                                                  //| 66: LDC 5
                                                  //| 67: CGTE
                                                  //| 68: SEL if_t_1 if_f_2
                                                  //| 69: RTN
                                                  //| 70: LD 3 0		; var Var(z,ListType(ProductType(TypeVariable(201,Some(U
                                                  //| PolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicC
                                                  //| onstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant
                                                  //| (Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))
                                                  //| )))
                                                  //| 71: JOIN
                                                  //| 72: LD 0 0		; var Var(l,ListType(ProductType(TypeVariable(201,Some(U
                                                  //| PolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicC
                                                  //| onstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp
                                                  //| (UPolymorphicConstant(Proj2),UVar(el)))))))))
                                                  //| 73: CAR
                                                  //| 74: LD 0 0		; var Var(l,ListType(ProductType(TypeVariable(201,Some(U
                                                  //| PolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicC
                                                  //| onstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp
                                                  //| (UPolymorphicConstant(Proj2),UVar(el)))))))))
                                                  //| 75: CDR
                                                  //| 76: LD 1 0		; var Var(go,ListType(ProductType(TypeVariable(201,Some(
                                                  //| UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphic
                                                  //| Constant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UAp
                                                  //| p(UPolymorphicConstant(Proj2),UVar(el)))))))) → ListType(ProductType(TypeV
                                                  //| ariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,
                                                  //| Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UP
                                                  //| olymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Prod
                                                  //| uctType(ℤ,ℤ))))))
                                                  //| 77: AP 1
                                                  //| 78: LD 2 0		; var Var(fun,ProductType(TypeVariable(201,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant
                                                  //| (Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolym
                                                  //| orphicConstant(Proj2),UVar(el))))))) → ListType(ProductType(TypeVariable(2
                                                  //| 01,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPol
                                                  //| ymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphi
                                                  //| cConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(�669 ��,ℤ))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConst
                                                  //| ant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)))
                                                  //| ,ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPo
                                                  //| lymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 79: AP 2
                                                  //| 80: JOIN
                                                  //| 81: LD 0 0		; var Var(l,ListType(ProductType(TypeVariable(201,Some(U
                                                  //| PolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicC
                                                  //| onstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp
                                                  //| (UPolymorphicConstant(Proj2),UVar(el)))))))))
                                                  //| 82: ATOM
                                                  //| 83: SEL if_t_5 if_f_6
                                                  //| 84: RTN
                                                  //| 85: LD 3 0		; var Var(list,ListType(ProductType(TypeVariable(201,Som
                                                  //| e(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorph
                                                  //| icConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),U
                                                  //| App(UPolymorphicConstant(Proj2),UVar(el)))))))))
                                                  //| 86: LD 0 0		; var Var(go,ListType(ProductType(TypeVariable(201,Some(
                                                  //| UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphic
                                                  //| Constant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UAp
                                                  //| p(UPolymorphicConstant(Proj2),UVar(el)))))))) → ListType(ProductType(TypeV
                                                  //| ariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,
                                                  //| Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UP
                                                  //| olymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Prod
                                                  //| uctType(ℤ,ℤ))))))
                                                  //| 87: AP 1
                                                  //| 88: RTN
                                                  //| 89: DUM 1
                                                  //| 90: LDF go
                                                  //| 91: LDF foldRightBody
                                                  //| 92: RAP 1
                                                  //| 93: RTN
                                                  //| 94: LDF fun_4
                                                  //| 95: RTN
                                                  //| 96: LDF fun_3
                                                  //| 97: RTN
                                                  //| 98: LD 1 0		; var Var(head,ProductType(TypeVariable(201,Some(UPolymo
                                                  //| rphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstan
                                                  //| t(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))))))
                                                  //| 99: LD 2 0		; var Var(fun,ProductType(TypeVariable(201,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant
                                                  //| (Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolym
                                                  //| orphicConstant(Proj2),UVar(el))))))) → ProductType(TypeVariable(201,Some(U
                                                  //| PolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicC
                                                  //| onstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant
                                                  //| (Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))
                                                  //| ))
                                                  //| 100: AP 1
                                                  //| 101: LD 0 0		; var Var(tail,ListType(ProductType(TypeVariable(201,Som
                                                  //| e(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorph
                                                  //| icConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConst
                                                  //| ant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ
                                                  //| ))))))
                                                  //| 102: CONS
                                                  //| 103: RTN
                                                  //| 104: LDF fun_9
                                                  //| 105: RTN
                                                  //| 106: LD 1 0		; var Var(list,ListType(ProductType(TypeVariable(201,Som
                                                  //| e(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorph
                                                  //| icConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),U
                                                  //| App(UPolymorphicConstant(Proj2),UVar(el)))))))))
                                                  //| 107: LDC 0
                                                  //| 108: LDF fun_8
                                                  //| 109: LD 2 4		; var Var(foldRight,ListType(ProductType(TypeVariable(20
                                                  //| 1,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPoly
                                                  //| morphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Pro
                                                  //| j2),UApp(UPolymorphicConstant(Proj2),UVar(el)))))))) → ListType(ProductTyp
                                                  //| e(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariab
                                                  //| le(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(
                                                  //| UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el)))
                                                  //| )),ProductType(ℤ,ℤ))))) → (ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))))) → ListType(ProductType(TypeVariable(20
                                                  //| 1,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPoly
                                                  //| morphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphic
                                                  //| Constant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(�
                                                  //| �,ℤ))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConsta
                                                  //| nt(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),
                                                  //| ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPol
                                                  //| ymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))) → ListType(
                                                  //| ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(
                                                  //| TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable
                                                  //| (204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),
                                                  //| UVar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 110: AP 3
                                                  //| 111: RTN
                                                  //| 112: LDF fun_7
                                                  //| 113: RTN
                                                  //| 114: LD 0 0		; var Var(world,ProductType(TypeVariable(114,Some(UApp(U
                                                  //| PolymorphicConstant(Proj1),UVar(world)))),TypeVariable(113,Some(UPolymorphic
                                                  //| Constant(Proj1)))))
                                                  //| 115: CAR
                                                  //| 116: RTN
                                                  //| 117: LD 0 0		; var Var(world,ProductType(TypeVariable(118,Some(UPolym
                                                  //| orphicConstant(Proj2))),ProductType(TypeVariable(121,Some(UApp(UPolymorphicC
                                                  //| onstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world))))),TypeVariable
                                                  //| (117,Some(UPolymorphicConstant(Proj1))))))
                                                  //| 118: CDR
                                                  //| 119: CAR
                                                  //| 120: RTN
                                                  //| 121: LD 1 0		; var Var(l,ListType(TypeVariable(137,Some(UPolymorphicC
                                                  //| onstant(Tail)))))
                                                  //| 122: CAR
                                                  //| 123: JOIN
                                                  //| 124: LD 1 0		; var Var(l,ListType(TypeVariable(137,Some(UPolymorphicC
                                                  //| onstant(Tail)))))
                                                  //| 125: CDR
                                                  //| 126: LD 0 0		; var Var(i,ℤ)
                                                  //| 127: LDC 1
                                                  //| 128: SUB
                                                  //| 129: LD 2 0		; var Var(go,ListType(TypeVariable(137,Some(UPolymorphic
                                                  //| Constant(Tail)))) → ℤ → TypeVariable(137,Some(UPolymorphicConstant(Tai
                                                  //| l))))
                                                  //| 130: AP 2
                                                  //| 131: JOIN
                                                  //| 132: LD 0 0		; var Var(i,ℤ)
                                                  //| 133: LDC 0
                                                  //| 134: CEQ
                                                  //| 135: SEL if_t_13 if_f_14
                                                  //| 136: RTN
                                                  //| 137: LDF fun_12
                                                  //| 138: RTN
                                                  //| 139: LD 1 0		; var Var(l,ListType(ListType(TypeVariable(137,Some(UPol
                                                  //| ymorphicConstant(Tail))))))
                                                  //| 140: CAR
                                                  //| 141: JOIN
                                                  //| 142: LD 1 0		; var Var(l,ListType(ListType(TypeVariable(137,Some(UPol
                                                  //| ymorphicConstant(Tail))))))
                                                  //| 143: CDR
                                                  //| 144: LD 0 0		; var Var(i,ℤ)
                                                  //| 145: LDC 1
                                                  //| 146: SUB
                                                  //| 147: LD 2 0		; var Var(go,ListType(ListType(TypeVariable(137,Some(UPo
                                                  //| lymorphicConstant(Tail))))) → ℤ → ListType(TypeVariable(137,Some(UPoly
                                                  //| morphicConstant(Tail)))))
                                                  //| 148: AP 2
                                                  //| 149: JOIN
                                                  //| 150: LD 0 0		; var Var(i,ℤ)
                                                  //| 151: LDC 0
                                                  //| 152: CEQ
                                                  //| 153: SEL if_t_16 if_f_17
                                                  //| 154: RTN
                                                  //| 155: LDF fun_15
                                                  //| 156: RTN
                                                  //| 157: LD 4 0		; var Var(world,ProductType(ListType(ListType(TypeVariab
                                                  //| le(137,Some(UPolymorphicConstant(Tail))))),TypeVariable(164,Some(UPolymorphi
                                                  //| cConstant(Proj1)))))
                                                  //| 158: CAR
                                                  //| 159: LD 2 0		; var Var(y,ℤ)
                                                  //| 160: LD 0 0		; var Var(go,ListType(ListType(TypeVariable(137,Some(UPo
                                                  //| lymorphicConstant(Tail))))) → ℤ → ListType(TypeVariable(137,Some(UPoly
                                                  //| morphicConstant(Tail)))))
                                                  //| 161: AP 2
                                                  //| 162: RTN
                                                  //| 163: DUM 1
                                                  //| 164: LDF go
                                                  //| 165: LDF elemAtBody
                                                  //| 166: RAP 1
                                                  //| 167: LD 2 0		; var Var(x,ℤ)
                                                  //| 168: LD 0 0		; var Var(go,ListType(TypeVariable(137,Some(UPolymorphic
                                                  //| Constant(Tail)))) → ℤ → TypeVariable(137,Some(UPolymorphicConstant(Tai
                                                  //| l))))
                                                  //| 169: AP 2
                                                  //| 170: RTN
                                                  //| 171: DUM 1
                                                  //| 172: LDF go
                                                  //| 173: LDF elemAtBody
                                                  //| 174: RAP 1
                                                  //| 175: RTN
                                                  //| 176: LDF fun_11
                                                  //| 177: RTN
                                                  //| 178: LDF fun_10
                                                  //| 179: RTN
                                                  //| 180: LD 0 0		; var Var(el,ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))))))
                                                  //| 181: CAR
                                                  //| 182: LD 0 0		; var Var(el,ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))))))
                                                  //| 183: CDR
                                                  //| 184: CAR
                                                  //| 185: LD 0 0		; var Var(el,ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))))))
                                                  //| 186: CDR
                                                  //| 187: CDR
                                                  //| 188: LDC 0
                                                  //| 189: LDC 0
                                                  //| 190: CONS
                                                  //| 191: CONS
                                                  //| 192: CONS
                                                  //| 193: CONS
                                                  //| 194: RTN
                                                  //| 195: LD 0 0		; var Var(world,ProductType(TypeVariable(175,Some(UPolym
                                                  //| orphicConstant(Proj2))),ProductType(TypeVariable(173,Some(UPolymorphicConsta
                                                  //| nt(Proj2))),ProductType(ListType(ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el)))))))),TypeVariable(172,Some(UPolymorphicConst
                                                  //| ant(Proj1)))))))
                                                  //| 196: CDR
                                                  //| 197: CDR
                                                  //| 198: CAR
                                                  //| 199: LDF fun_18
                                                  //| 200: LD 1 5		; var Var(map,ListType(ProductType(TypeVariable(201,Some
                                                  //| (UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphi
                                                  //| cConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UA
                                                  //| pp(UPolymorphicConstant(Proj2),UVar(el)))))))) → (ProductType(TypeVariable
                                                  //| (201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UP
                                                  //| olymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymorphicConstant(
                                                  //| Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))))) → ProductType(TypeV
                                                  //| ariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,
                                                  //| Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UP
                                                  //| olymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Prod
                                                  //| uctType(ℤ,ℤ))))) → ListType(ProductType(TypeVariable(201,Some(UPolymor
                                                  //| phicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant
                                                  //| (Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2)
                                                  //| ,UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 201: AP 2
                                                  //| 202: RTN
                                                  //| 203: LD 0 0		; var Var(world,ProductType(TypeVariable(219,Some(UPolym
                                                  //| orphicConstant(Proj2))),ProductType(TypeVariable(217,Some(UPolymorphicConsta
                                                  //| nt(Proj2))),ProductType(TypeVariable(215,Some(UPolymorphicConstant(Proj2))),
                                                  //| TypeVariable(223,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCons
                                                  //| tant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(world))))))))))
                                                  //| 204: CDR
                                                  //| 205: CDR
                                                  //| 206: CDR
                                                  //| 207: RTN
                                                  //| 208: LD 0 0		; var Var(obj,ℤ)
                                                  //| 209: LDC 0
                                                  //| 210: CEQ
                                                  //| 211: RTN
                                                  //| 212: LD 0 0		; var Var(obj,ℤ)
                                                  //| 213: LDC 1
                                                  //| 214: CEQ
                                                  //| 215: RTN
                                                  //| 216: LD 0 0		; var Var(obj,ℤ)
                                                  //| 217: LDC 2
                                                  //| 218: CEQ
                                                  //| 219: RTN
                                                  //| 220: LD 0 0		; var Var(obj,ℤ)
                                                  //| 221: LDC 3
                                                  //| 222: CEQ
                                                  //| 223: RTN
                                                  //| 224: LD 0 0		; var Var(obj,ℤ)
                                                  //| 225: LDC 4
                                                  //| 226: CEQ
                                                  //| 227: RTN
                                                  //| 228: LD 0 0		; var Var(obj,ℤ)
                                                  //| 229: LDC 5
                                                  //| 230: CEQ
                                                  //| 231: RTN
                                                  //| 232: LD 0 0		; var Var(obj,ℤ)
                                                  //| 233: LDC 6
                                                  //| 234: CEQ
                                                  //| 235: RTN
                                                  //| 236: LD 0 0		; var Var(char,ProductType(ℤ,TypeVariable(247,Some(UPo
                                                  //| lymorphicConstant(Proj1)))))
                                                  //| 237: CAR
                                                  //| 238: RTN
                                                  //| 239: LD 0 0		; var Var(char,ProductType(TypeVariable(252,Some(UPolymo
                                                  //| rphicConstant(Proj2))),ProductType(TypeVariable(255,Some(UApp(UPolymorphicCo
                                                  //| nstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVar(char))))),TypeVariable(2
                                                  //| 51,Some(UPolymorphicConstant(Proj1))))))
                                                  //| 240: CDR
                                                  //| 241: CAR
                                                  //| 242: RTN
                                                  //| 243: LD 0 0		; var Var(char,ProductType(TypeVariable(261,Some(UPolymo
                                                  //| rphicConstant(Proj2))),ProductType(TypeVariable(259,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(265,Some(UApp(UPolymorphicConstant(Proj1
                                                  //| ),UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(cha
                                                  //| r)))))),TypeVariable(258,Some(UPolymorphicConstant(Proj1)))))))
                                                  //| 244: CDR
                                                  //| 245: CDR
                                                  //| 246: CAR
                                                  //| 247: RTN
                                                  //| 248: LD 0 0		; var Var(char,ProductType(TypeVariable(273,Some(UPolymo
                                                  //| rphicConstant(Proj2))),ProductType(TypeVariable(271,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(269,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(278,Some(UApp(UPolymorphicConstant(Proj1),UApp(UPoly
                                                  //| morphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCon
                                                  //| stant(Proj2),UVar(char))))))),TypeVariable(268,Some(UPolymorphicConstant(Pro
                                                  //| j1))))))))
                                                  //| 249: CDR
                                                  //| 250: CDR
                                                  //| 251: CDR
                                                  //| 252: CAR
                                                  //| 253: RTN
                                                  //| 254: LD 0 0		; var Var(char,ProductType(TypeVariable(286,Some(UPolymo
                                                  //| rphicConstant(Proj2))),ProductType(TypeVariable(284,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(282,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(280,Some(UPolymorphicConstant(Proj2))),TypeVariable(
                                                  //| 291,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),U
                                                  //| App(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(char))
                                                  //| ))))))))))
                                                  //| 255: CDR
                                                  //| 256: CDR
                                                  //| 257: CDR
                                                  //| 258: CDR
                                                  //| 259: RTN
                                                  //| 260: LD 0 0		; var Var(ghost,ProductType(ℤ,TypeVariable(247,Some(UP
                                                  //| olymorphicConstant(Proj1)))))
                                                  //| 261: LD 1 18		; var Var(vitality,ProductType(ℤ,TypeVariable(
                                                  //| 247,Some(UPolymorphicConstant(Proj1)))) → ℤ)
                                                  //| 262: AP 1
                                                  //| 263: LDC 1
                                                  //| 264: CEQ
                                                  //| 265: RTN
                                                  //| 266: LD 0 0		; var Var(ghost,ProductType(ℤ,TypeVariable(247,Some(UP
                                                  //| olymorphicConstant(Proj1)))))
                                                  //| 267: LD 1 18		; var Var(vitality,ProductType(ℤ,TypeVariable(
                                                  //| 247,Some(UPolymorphicConstant(Proj1)))) → ℤ)
                                                  //| 268: AP 1
                                                  //| 269: LDC 2
                                                  //| 270: CEQ
                                                  //| 271: RTN
                                                  //| 272: LD 0 0		; var Var(lambdaMan,ProductType(ℤ,TypeVariable(302,Som
                                                  //| e(UPolymorphicConstant(Proj1)))))
                                                  //| 273: CAR
                                                  //| 274: RTN
                                                  //| 275: LDC 1
                                                  //| 276: LD 0 0		; var Var(lambdaMan,ProductType(ℤ,TypeVariable(302,Som
                                                  //| e(UPolymorphicConstant(Proj1)))))
                                                  //| 277: LD 1 25		; var Var(powerLeft,ProductType(ℤ,TypeVariable
                                                  //| (302,Some(UPolymorphicConstant(Proj1)))) → ℤ)
                                                  //| 278: AP 1
                                                  //| 279: LDC 0
                                                  //| 280: CEQ
                                                  //| 281: SUB
                                                  //| 282: RTN
                                                  //| 283: LD 0 0		; var Var(obj,ℤ)
                                                  //| 284: LDC 0
                                                  //| 285: CEQ
                                                  //| 286: RTN
                                                  //| 287: LD 0 0		; var Var(obj,ℤ)
                                                  //| 288: LDC 1
                                                  //| 289: CEQ
                                                  //| 290: RTN
                                                  //| 291: LD 0 0		; var Var(obj,ℤ)
                                                  //| 292: LDC 2
                                                  //| 293: CEQ
                                                  //| 294: RTN
                                                  //| 295: LD 0 0		; var Var(obj,ℤ)
                                                  //| 296: LDC 3
                                                  //| 297: CEQ
                                                  //| 298: RTN
                                                  //| 299: LDC 42
                                                  //| 300: LD 0 0		; var Var(go,ℤ → TypeVariable(321,Some(UApp(UVar(go)
                                                  //| ,UMonomorphicConstant(LiteralInt(42))))))
                                                  //| 301: AP 1
                                                  //| 302: RTN
                                                  //| 
                                                  //| 195: Var(world_ghostsStatus,ProductType(TypeVariable(175,Some(UPolymorphicCo
                                                  //| nstant(Proj2))),ProductType(TypeVariable(173,Some(UPolymorphicConstant(Proj2
                                                  //| ))),ProductType(ListType(ProductType(TypeVariable(201,Some(UPolymorphicConst
                                                  //| ant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)))
                                                  //| ,TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCon
                                                  //| stant(Proj2),UVar(el)))))))),TypeVariable(172,Some(UPolymorphicConstant(Proj
                                                  //| 1)))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(
                                                  //| Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),Pro
                                                  //| ductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymo
                                                  //| rphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 89: Var(fun_4,(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVaria
                                                  //| ble(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj
                                                  //| 2),UVar(el))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphi
                                                  //| cConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Pr
                                                  //| oj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UA
                                                  //| pp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → Lis
                                                  //| tType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Produc
                                                  //| tType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVa
                                                  //| riable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(P
                                                  //| roj2),UVar(el))))),ProductType(ℤ,ℤ)))))) → ListType(ProductType(TypeVa
                                                  //| riable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,S
                                                  //| ome(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPo
                                                  //| lymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Produ
                                                  //| ctType(ℤ,ℤ))))))
                                                  //| 85: Var(foldRightBody,UnitType)
                                                  //| 98: Var(fun_9,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → ListType(Pr
                                                  //| oductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(Ty
                                                  //| peVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(2
                                                  //| 04,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UV
                                                  //| ar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 171: Var(fun_11,ℤ → TypeVariable(137,Some(UPolymorphicConstant(Tail))))
                                                  //| 208: Var(isWall,ℤ → BooleanType)
                                                  //| 106: Var(fun_7,(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2
                                                  //| ))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVari
                                                  //| able(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Pro
                                                  //| j2),UVar(el))))))) → ProductType(TypeVariable(201,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → ListType(Pr
                                                  //| oductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(Ty
                                                  //| peVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(2
                                                  //| 04,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UV
                                                  //| ar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 266: Var(isInvisible,ProductType(ℤ,TypeVariable(247,Some(UPolymorphicConst
                                                  //| ant(Proj1)))) → BooleanType)
                                                  //| 163: Var(elemAtBody,UnitType)
                                                  //| 272: Var(powerLeft,ProductType(ℤ,TypeVariable(302,Some(UPolymorphicConstan
                                                  //| t(Proj1)))) → ℤ)
                                                  //| 275: Var(isInPowerMode,ProductType(ℤ,TypeVariable(302,Some(UPolymorphicCon
                                                  //| stant(Proj1)))) → BooleanType)
                                                  //| 236: Var(vitality,ProductType(ℤ,TypeVariable(247,Some(UPolymorphicConstant
                                                  //| (Proj1)))) → ℤ)
                                                  //| 124: Var(if_f_14,UnitType → TypeVariable(137,Some(UPolymorphicConstant(Tai
                                                  //| l))))
                                                  //| 117: Var(world_lambdaStatus,ProductType(TypeVariable(118,Some(UPolymorphicCo
                                                  //| nstant(Proj2))),ProductType(TypeVariable(121,Some(UApp(UPolymorphicConstant(
                                                  //| Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world))))),TypeVariable(117,Som
                                                  //| e(UPolymorphicConstant(Proj1))))) → TypeVariable(121,Some(UApp(UPolymorphi
                                                  //| cConstant(Proj1),UApp(UPolymorphicConstant(Proj2),UVar(world))))))
                                                  //| 41: Var(to,ℤ → TypeVariable(321,Some(UApp(UVar(go),UMonomorphicConstant(
                                                  //| LiteralInt(42))))))
                                                  //| 212: Var(isEmptyField,ℤ → BooleanType)
                                                  //| 232: Var(isGhostStart,ℤ → BooleanType)
                                                  //| 254: Var(score,ProductType(TypeVariable(286,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(284,Some(UPolymorphicConstant(Proj2))),ProductTy
                                                  //| pe(TypeVariable(282,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVaria
                                                  //| ble(280,Some(UPolymorphicConstant(Proj2))),TypeVariable(291,Some(UApp(UPolym
                                                  //| orphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicCons
                                                  //| tant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(char))))))))))) → TypeVa
                                                  //| riable(291,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(P
                                                  //| roj2),UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar
                                                  //| (char))))))))
                                                  //| 299: Var(main,UnitType)
                                                  //| 96: Var(foldRight,ListType(ProductType(TypeVariable(201,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)
                                                  //| )),TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicC
                                                  //| onstant(Proj2),UVar(el)))))))) → ListType(ProductType(TypeVariable(201,Som
                                                  //| e(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorph
                                                  //| icConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConst
                                                  //| ant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ
                                                  //| ))))) → (ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(
                                                  //| 204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),U
                                                  //| Var(el))))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(U
                                                  //| PolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → ListTyp
                                                  //| e(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductTyp
                                                  //| e(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariab
                                                  //| le(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2
                                                  //| ),UVar(el))))),ProductType(ℤ,ℤ)))))) → ListType(ProductType(TypeVariab
                                                  //| le(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,Some(
                                                  //| UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymo
                                                  //| rphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductTy
                                                  //| pe(ℤ,ℤ))))))
                                                  //| 228: Var(isLambdaStart,ℤ → BooleanType)
                                                  //| 139: Var(if_t_16,UnitType → ListType(TypeVariable(137,Some(UPolymorphicCon
                                                  //| stant(Tail)))))
                                                  //| 176: Var(fun_10,ℤ → ℤ → TypeVariable(137,Some(UPolymorphicConstant(T
                                                  //| ail))))
                                                  //| 178: Var(world_itemAt,ProductType(ListType(ListType(TypeVariable(137,Some(UP
                                                  //| olymorphicConstant(Tail))))),TypeVariable(164,Some(UPolymorphicConstant(Proj
                                                  //| 1)))) → ℤ → ℤ → TypeVariable(137,Some(UPolymorphicConstant(Tail)))
                                                  //| )
                                                  //| 63: Var(if_f_2,UnitType → ℤ)
                                                  //| 47: Var(listTest,TypeVariable(43,Some(UAbs(n,None,UApp(UPolymorphicConstant(
                                                  //| Head),UApp(UPolymorphicConstant(Tail),UApp(UApp(UPolymorphicConstant(Cons),U
                                                  //| MonomorphicConstant(LiteralInt(11))),UApp(UApp(UPolymorphicConstant(Cons),UM
                                                  //| onomorphicConstant(LiteralInt(42))),UPolymorphicConstant(Empty)))))))) → �669 ��)
                                                  //| 216: Var(isPill,ℤ → BooleanType)
                                                  //| 114: Var(world_map,ProductType(TypeVariable(114,Some(UApp(UPolymorphicConsta
                                                  //| nt(Proj1),UVar(world)))),TypeVariable(113,Some(UPolymorphicConstant(Proj1)))
                                                  //| ) → TypeVariable(114,Some(UApp(UPolymorphicConstant(Proj1),UVar(world)))))
                                                  //| 
                                                  //| 180: Var(fun_18,ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2
                                                  //| ))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),TypeVari
                                                  //| able(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Pro
                                                  //| j2),UVar(el))))))) → ProductType(TypeVariable(201,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ)))))
                                                  //| 65: Var(foo,ℤ → ℤ)
                                                  //| 283: Var(isUp,ℤ → BooleanType)
                                                  //| 55: Var(if_t_1,UnitType → ℤ)
                                                  //| 94: Var(fun_3,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstan
                                                  //| t(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),P
                                                  //| roductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPoly
                                                  //| morphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))) → (ProductTyp
                                                  //| e(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariab
                                                  //| le(199,Some(UPolymorphicConstant(Proj2))),TypeVariable(204,Some(UApp(UPolymo
                                                  //| rphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))))) → Lis
                                                  //| tType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(Proj2))),Produc
                                                  //| tType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVa
                                                  //| riable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(P
                                                  //| roj2),UVar(el))))),ProductType(ℤ,ℤ))))) → ListType(ProductType(TypeVar
                                                  //| iable(201,Some(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(199,So
                                                  //| me(UPolymorphicConstant(Proj2))),ProductType(TypeVariable(204,Some(UApp(UPol
                                                  //| ymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(el))))),Produc
                                                  //| tType(ℤ,ℤ)))))) → ListType(ProductType(TypeVariable(201,Some(UPolymorp
                                                  //| hicConstant(Proj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(
                                                  //| Proj2))),ProductType(TypeVariable(204,Some(UApp(UPolymorphicConstant(Proj2),
                                                  //| UApp(UPolymorphicConstant(Proj2),UVar(el))))),ProductType(ℤ,ℤ))))))
                                                  //| 203: Var(world_fruitStatus,ProductType(TypeVariable(219,Some(UPolymorphicCon
                                                  //| stant(Proj2))),ProductType(TypeVariable(217,Some(UPolymorphicConstant(Proj2)
                                                  //| )),ProductType(TypeVariable(215,Some(UPolymorphicConstant(Proj2))),TypeVaria
                                                  //| ble(223,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj
                                                  //| 2),UApp(UPolymorphicConstant(Proj2),UVar(world))))))))) → TypeVariable(223
                                                  //| ,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UApp
                                                  //| (UPolymorphicConstant(Proj2),UVar(world)))))))
                                                  //| 243: Var(direction,ProductType(TypeVariable(261,Some(UPolymorphicConstant(Pr
                                                  //| oj2))),ProductType(TypeVariable(259,Some(UPolymorphicConstant(Proj2))),Produ
                                                  //| ctType(TypeVariable(265,Some(UApp(UPolymorphicConstant(Proj1),UApp(UPolymorp
                                                  //| hicConstant(Proj2),UApp(UPolymorphicConstant(Proj2),UVar(char)))))),TypeVari
                                                  //| able(258,Some(UPolymorphicConstant(Proj1)))))) → TypeVariable(265,Some(UAp
                                                  //| p(UPolymorphicConstant(Proj1),UApp(UPolymorphicConstant(Proj2),UApp(UPolymor
                                                  //| phicConstant(Proj2),UVar(char)))))))
                                                  //| 287: Var(isRight,ℤ → BooleanType)
                                                  //| 150: Var(fun_15,ℤ → ListType(TypeVariable(137,Some(UPolymorphicConstant(
                                                  //| Tail)))))
                                                  //| 81: Var(go,ListType(ProductType(TypeVariable(201,Some(UPolymorphicConstant(P
                                                  //| roj2))),ProductType(TypeVariable(199,Some(UPolymorphicConstant(Proj2))),Type
                                                  //| Variable(204,Some(UApp(UPolymorphicConstant(Proj2),UApp(UPolymorphi
                                                  //| Output exceeds cutoff limit.

}