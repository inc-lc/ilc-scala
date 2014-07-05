package ilc
package feature
package let

import language._

object StaticCaching {
  val v = new Bacchus with let.ANormalFormAdapter with integers.ImplicitSyntaxSugar with inference.LetInference
    with BetaReduction with Pretty
    with products.StdLib
    //with inference.SyntaxSugar //Or with:
    with inference.LetSyntaxSugar {
      outer =>
      val aNormalizer = new feature.let.ANormalFormStateful {
        val mySyntax: outer.type = outer
      }
    }                                             //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalFormAdapter with ilc.f
                                                  //| eature.integers.ImplicitSyntaxSugar with ilc.feature.inference.LetInference 
                                                  //| with ilc.feature.let.BetaReduction with ilc.feature.let.Pretty with ilc.feat
                                                  //| ure.products.StdLib with ilc.feature.inference.LetSyntaxSugar{val aNormalize
                                                  //| r: ilc.feature.let.ANormalFormStateful{val mySyntax: ilc.feature.let.StaticC
                                                  //| aching.<refinement>.type}} = ilc.feature.let.StaticCaching$$anonfun$main$1$$
                                                  //| anon$1@6a4f0359
  val v1 = new AddCaches {
    val mySyntax: v.type = v
  }                                               //> v1  : ilc.feature.let.AddCaches{val mySyntax: ilc.feature.let.StaticCaching.
                                                  //| <refinement>.type} = ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2@7
                                                  //| 3cd15da

  import v._
  import v1.addCaches

  val testBeta1 =
    'f2 ->:
	    letS(
	      'f -> ('x ->: 'y ->: 'f2('x)('y)),
	      'f1 -> ('x ->: 'y ->: pair('x)('y)),
	      'h -> ('x ->: 'y ->: let('g, 'f('x))(PlusInt('g('y))('g('y))))
	    ) { 'h }                              //> testBeta1  : ilc.feature.let.StaticCaching.v.UntypedTerm = UAbs(f2,None,ULet
                                                  //| (f,UAbs(x,None,UAbs(y,None,UApp(UApp(UVar(f2),UVar(x)),UVar(y)))),ULet(f1,UA
                                                  //| bs(x,None,UAbs(y,None,UApp(UApp(UPolymorphicConstant(Pair),UVar(x)),UVar(y))
                                                  //| )),ULet(h,UAbs(x,None,UAbs(y,None,ULet(g,UApp(UVar(f),UVar(x)),UApp(UApp(UMo
                                                  //| nomorphicConstant(PlusInt),UApp(UVar(g),UVar(y))),UApp(UVar(g),UVar(y)))))),
                                                  //| UVar(h)))))
  "\n" + pretty(normalize(testBeta1))             //> res0: String = "
                                                  //| λf2lit_1.
                                                  //| λx_2.
                                                  //| λy_3.
                                                  //|   PlusInt
                                                  //|     (f2lit_1
                                                  //|        x_2
                                                  //|        y_3)
                                                  //|     (f2lit_1
                                                  //|        x_2
                                                  //|        y_3)"
  val testBeta1Bis =
    'f2 ->:
      letS(
        'f -> ('x ->: 'y ->: 'f2('x)('y)),
        'h -> ('x ->: 'y ->:
          letS(
            'g -> 'f('x),
            'i -> 'g('y)
          ) {
              PlusInt('i)('i)
            })
      ) { 'h }                                    //> testBeta1Bis  : ilc.feature.let.StaticCaching.v.UntypedTerm = UAbs(f2,None,
                                                  //| ULet(f,UAbs(x,None,UAbs(y,None,UApp(UApp(UVar(f2),UVar(x)),UVar(y)))),ULet(
                                                  //| h,UAbs(x,None,UAbs(y,None,ULet(g,UApp(UVar(f),UVar(x)),ULet(i,UApp(UVar(g),
                                                  //| UVar(y)),UApp(UApp(UMonomorphicConstant(PlusInt),UVar(i)),UVar(i)))))),UVar
                                                  //| (h))))
  "\n" + pretty(normalize(testBeta1Bis))          //> res1: String = "
                                                  //| λf2lit_1.
                                                  //| λx_2.
                                                  //| λy_3.
                                                  //|   i_4 =
                                                  //|     f2lit_1
                                                  //|       x_2
                                                  //|       y_3;
                                                  //|   PlusInt
                                                  //|     i_4
                                                  //|     i_4"
  val testBeta2 =
    'f ->:
      letS(
        'h -> ('x ->: 'y ->: let('g, 'f('x))('g('y)))
      ) { 'h }                                    //> testBeta2  : ilc.feature.let.StaticCaching.v.UntypedTerm = UAbs(f,None,ULet
                                                  //| (h,UAbs(x,None,UAbs(y,None,ULet(g,UApp(UVar(f),UVar(x)),UApp(UVar(g),UVar(y
                                                  //| ))))),UVar(h)))
  "\n" + pretty(normalize(testBeta2))             //> res2: String = "
                                                  //| λf_1.
                                                  //| λx_2.
                                                  //| λy_3.
                                                  //|   f_1
                                                  //|     x_2
                                                  //|     y_3"
  val testBeta3 =
    'f ->:
      letS(
        'h -> ('x ->: 'y ->: let('g, 'f('x))(PlusInt('g('y))('g('y))))
      ) { 'h }                                    //> testBeta3  : ilc.feature.let.StaticCaching.v.UntypedTerm = UAbs(f,None,ULet
                                                  //| (h,UAbs(x,None,UAbs(y,None,ULet(g,UApp(UVar(f),UVar(x)),UApp(UApp(UMonomorp
                                                  //| hicConstant(PlusInt),UApp(UVar(g),UVar(y))),UApp(UVar(g),UVar(y)))))),UVar(
                                                  //| h)))
  "\n" + pretty(normalize(testBeta3))             //> res3: String = "
                                                  //| λf_1.
                                                  //| λx_2.
                                                  //| λy_3.
                                                  //|   g_4 =
                                                  //|     f_1
                                                  //|       x_2;
                                                  //|   PlusInt
                                                  //|     (g_4
                                                  //|        y_3)
                                                  //|     (g_4
                                                  //|        y_3)"
  "\n" + pretty(aNormalizeTerm(normalize(testBeta3)))
                                                  //> res4: String = "
                                                  //| λf_1.
                                                  //| λx_2.
                                                  //| λy_3.
                                                  //|   a_1 =
                                                  //|     f_1
                                                  //|       x_2;
                                                  //|   a_2 =
                                                  //|     a_1
                                                  //|       y_3;
                                                  //|   a_3 =
                                                  //|     PlusInt
                                                  //|       a_2
                                                  //|       a_2;
                                                  //|   a_3"
  val test1 =
    letS(
      'id -> ('x ->: 'x),
      'id_i -> ('x ->: 'x),
      'id_i2 -> ('x ->: 'x),
      'apply -> ('f ->: 'x ->: 'f('x))
    ) {
        'id('apply, 'id_i, 'id_i2(3: Term))
      }                                           //> test1  : ilc.feature.let.StaticCaching.v.UntypedTerm = ULet(id,UAbs(x,None,U
                                                  //| Var(x)),ULet(id_i,UAbs(x,None,UVar(x)),ULet(id_i2,UAbs(x,None,UVar(x)),ULet(
                                                  //| apply,UAbs(f,None,UAbs(x,None,UApp(UVar(f),UVar(x)))),UApp(UApp(UApp(UVar(id
                                                  //| ),UVar(apply)),UVar(id_i)),UApp(UVar(id_i2),UMonomorphicConstant(LiteralInt(
                                                  //| 3))))))))
  //val t = test1
  val test3 =
    let('x, ifThenElse(True, 1, 2): Term)('x)     //> test3  : ilc.feature.let.StaticCaching.v.UntypedTerm = ULet(x,UMonomorphicCo
                                                  //| nstant(App(App(App(IfThenElse(ℤ),True),Abs(Var(unit,UnitType),LiteralInt(1
                                                  //| ))),Abs(Var(unit,UnitType),LiteralInt(2)))),UVar(x))
  "\n" + pretty(test3)                            //> res0: String = "
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     (λunit.
                                                  //|        LiteralInt(1))
                                                  //|     (λunit.
                                                  //|        LiteralInt(2));
                                                  //| x"
  "\n" + pretty(addCaches(test3))                 //> res1: String = "
                                                  //| aTot_1 =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True;
                                                  //| a_1 =
                                                  //|   Proj1((UnitType → ℤ) → (UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_1;
                                                  //| a_2 =
                                                  //|   λunit.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       LiteralInt(1)
                                                  //|       UnitTerm;
                                                  //| aTot_3 =
                                                  //|   a_1
                                                  //|     a_2;
                                                  //| a_3 =
                                                  //|   Proj1((UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_3;
                                                  //| a_4 =
                                                  //|   λunit.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       LiteralInt(2)
                                                  //|       UnitTerm;
                                                  //| aTot_5 =
                                                  //|   a_3
                                                  //|     a_4;
                                                  //| a_5 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_5;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|  λx5lit.
                                                  //|  λx6lit.
                                                  //|    Pair(ℤ, ProductType(ℤ,ProductType(UnitType → ProductType(ℤ,UnitTy
                                                  //| pe),ProductType((UnitType → ℤ) → ℤ,ProductType(UnitType → ProductT
                                                  //| ype(ℤ,UnitType),(UnitType → ℤ) → (UnitType → ℤ) → ℤ)))))
                                                  //|      x1lit
                                                  //|      (Pair(ℤ, ProductType(UnitType → ProductType(ℤ,UnitType),ProductTy
                                                  //| pe((UnitType → ℤ) → ℤ,ProductType(UnitType → ProductType(ℤ,UnitT
                                                  //| ype),(UnitType → ℤ) → (UnitType → ℤ) → ℤ))))
                                                  //|         x2lit
                                                  //|         (Pair(UnitType → ProductType(ℤ,UnitType), ProductType((UnitType 
                                                  //| → ℤ) → ℤ,ProductType(UnitType → ProductType(ℤ,UnitType),(UnitTyp
                                                  //| e → ℤ) → (UnitType → ℤ) → ℤ)))
                                                  //|            x3lit
                                                  //|            (Pair((UnitType → ℤ) → ℤ, ProductType(UnitType → Produc
                                                  //| tType(ℤ,UnitType),(UnitType → ℤ) → (UnitType → ℤ) → ℤ))
                                                  //|               x4lit
                                                  //|               (Pair(UnitType → ProductType(ℤ,UnitType), (UnitType → �
                                                  //| �) → (UnitType → ℤ) → ℤ)
                                                  //|                  x5lit
                                                  //|                  x6lit)))))
                                                  //|   a_5
                                                  //|   a_5
                                                  //|   a_4
                                                  //|   a_3
                                                  //|   a_2
                                                  //|   a_1"
  "\n" + pretty(addCaches(test1))                 //> res2: String = "
                                                  //| id =
                                                  //|   λx.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair((ℤ → ℤ) → ℤ → ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       x
                                                  //|       UnitTerm;
                                                  //| id_i =
                                                  //|   λx.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       x
                                                  //|       UnitTerm;
                                                  //| apply =
                                                  //|   λf.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ → ProductType(ℤ,ℤ), UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       (λx.
                                                  //|          aTot_6 =
                                                  //|            f
                                                  //|              x;
                                                  //|          aTot_6 =
                                                  //|            Proj1(ProductType(ℤ,UnknownType()), UnknownType())
                                                  //|              aTot_6;
                                                  //|          aTot_6 =
                                                  //|            Proj1(ℤ, UnknownType())
                                                  //|              aTot_6;
                                                  //|          a_6 =
                                                  //|            Proj1(ℤ, UnknownType())
                                                  //|              aTot_6;
                                                  //|          (λx1lit.
                                                  //|           λx2lit.
                                                  //|             Pair(ℤ, ℤ)
                                                  //|               x1lit
                                                  //|               x2lit)
                                                  //|            aTot_6
                                                  //|            aTot_6)
                                                  //|       UnitTerm;
                                                  //| aTot_7 =
                                                  //|   id
                                                  //|     apply;
                                                  //| a_7 =
                                                  //|   Proj1((ℤ → ℤ) → ℤ → ℤ, UnknownType())
                                                  //|     aTot_7;
                                                  //| aTot_8 =
                                                  //|   a_7
                                                  //|     id_i;
                                                  //| a_8 =
                                                  //|   Proj1(ℤ → ℤ, UnknownType())
                                                  //|     aTot_8;
                                                  //| aTot_9 =
                                                  //|   id_i
                                                  //|     LiteralInt(3);
                                                  //| a_9 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_9;
                                                  //| aTot_10 =
                                                  //|   a_8
                                                  //|     a_9;
                                                  //| a_10 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_10;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|  λx5lit.
                                                  //|  λx6lit.
                                                  //|  λx7lit.
                                                  //|  λx8lit.
                                                  //|    Pair(ℤ, ProductType(ℤ,ProductType(ℤ,ProductType(ℤ → ℤ,Product
                                                  //| Type((ℤ → ℤ) → ℤ → ℤ,ProductType((ℤ → ℤ) → ℤ → ℤ
                                                  //| ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) �912 �� ℤ → ℤ)))))))
                                                  //|      x1lit
                                                  //|      (Pair(ℤ, ProductType(ℤ,ProductType(ℤ → ℤ,ProductType((ℤ →
                                                  //|  ℤ) → ℤ → ℤ,ProductType((ℤ → ℤ) → ℤ → ℤ,ProductType(
                                                  //| ℤ → ℤ,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → �
                                                  //| �))))))
                                                  //|         x2lit
                                                  //|         (Pair(ℤ, ProductType(ℤ → ℤ,ProductType((ℤ → ℤ) → ℤ
                                                  //|  → ℤ,ProductType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,(
                                                  //| (ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))))
                                                  //|            x3lit
                                                  //|            (Pair(ℤ → ℤ, ProductType((ℤ → ℤ) → ℤ → ℤ,Prod
                                                  //| uctType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ)
                                                  //|  → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ))))
                                                  //|               x4lit
                                                  //|               (Pair((ℤ → ℤ) → ℤ → ℤ, ProductType((ℤ → ℤ)
                                                  //|  → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → ℤ) →
                                                  //|  (ℤ → ℤ) → ℤ → ℤ)))
                                                  //|                  x5lit
                                                  //|                  (Pair((ℤ → ℤ) → ℤ → ℤ, ProductType(ℤ → �
                                                  //| �,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ))
                                                  //|                     x6lit
                                                  //|                     (Pair(ℤ → ℤ, ((ℤ → ℤ) → ℤ → ℤ) → (
                                                  //| ℤ → ℤ) → ℤ → ℤ)
                                                  //|                        x7lit
                                                  //|                        x8lit)))))))
                                                  //|   a_10
                                                  //|   a_10
                                                  //|   a_9
                                                  //|   a_8
                                                  //|   a_7
                                                  //|   apply
                                                  //|   id_i
                                                  //|   id"
                                                  /*
  val bindings = createBindings()
  val normalT = aNormalizeAddCaches(t, bindings)
  val withAdaptedCallers = adaptCallers(normalT)
  val adaptedBindings = bindings.bindings.toList map {
	  case (t, v) => (adaptCallers(t), v)
  }
  adaptedBindings map {
   case (a, b) => s"\n$b:\n ${pretty(a)}\n"
  } mkString ""
                                                  */
}