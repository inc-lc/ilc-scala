package ilc
package feature
package let

import language._

object StaticCaching {
  val v = new Bacchus with integers.ImplicitSyntaxSugar with inference.LetInference
    with BetaReduction with Pretty
    with products.StdLib
    //with inference.SyntaxSugar //Or with:
    with inference.LetSyntaxSugar                 //> v  : ilc.language.Bacchus with ilc.feature.integers.ImplicitSyntaxSugar with
                                                  //|  ilc.feature.inference.LetInference with ilc.feature.let.BetaReduction with 
                                                  //| ilc.feature.let.Pretty with ilc.feature.products.StdLib with ilc.feature.inf
                                                  //| erence.LetSyntaxSugar = ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$
                                                  //| 1@1430eb11
    /*
    with let.ANormalFormAdapter {
      outer =>
      val aNormalizer = new feature.let.ANormalFormStateful {
        val mySyntax: outer.type = outer
      }
    }
  */
  import v._
  /*
  val cacher = new AddCaches {
    val mySyntax: v.type = v
  }

  */
   val cacher = new AddCaches2 {
    val mySyntax: v.type = v
  }                                               //> cacher  : ilc.feature.let.AddCaches2{val mySyntax: ilc.feature.let.StaticCac
                                                  //| hing.<refinement>.type} = ilc.feature.let.StaticCaching$$anonfun$main$1$$ano
                                                  //| n$2@1b28af8
  import cacher._
  import aNormalizer._

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
                                                  //|       a_2;
                                                  //|   a_4 =
                                                  //|     a_3
                                                  //|       a_2;
                                                  //|   a_4"
  val test1 =
    letS(
      'id -> ('x ->: 'x),
      'id_i -> ('x ->: 'x),
      'id_i2 -> ('x ->: 'x),
      'apply -> ('f ->: 'x ->: 'f('x))
    ) {
        'id('apply, 'id_i, 'id_i2(3: Term))
      }                                           //> test1  : ilc.feature.let.StaticCaching.v.UntypedTerm = ULet(id,UAbs(x,None,
                                                  //| UVar(x)),ULet(id_i,UAbs(x,None,UVar(x)),ULet(id_i2,UAbs(x,None,UVar(x)),ULe
                                                  //| t(apply,UAbs(f,None,UAbs(x,None,UApp(UVar(f),UVar(x)))),UApp(UApp(UApp(UVar
                                                  //| (id),UVar(apply)),UVar(id_i)),UApp(UVar(id_i2),UMonomorphicConstant(Literal
                                                  //| Int(3))))))))
  //val t = test1
  val test3 =
    let('x, ifThenElse(True, 1, 2): Term)('x)     //> test3  : ilc.feature.let.StaticCaching.v.UntypedTerm = ULet(x,UMonomorphicC
                                                  //| onstant(App(App(App(IfThenElse(ℤ),True),Abs(Var(unit,UnitType),LiteralInt
                                                  //| (1))),Abs(Var(unit,UnitType),LiteralInt(2)))),UVar(x))
  "\n" + pretty(test3)                            //> res5: String = "
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     (λunit.
                                                  //|        LiteralInt(1))
                                                  //|     (λunit.
                                                  //|        LiteralInt(2));
                                                  //| x"
  "\n" + pretty(etaExpandPrimitives(test3))       //> res6: String = "
                                                  //| x =
                                                  //|   (λeta_1.
                                                  //|    λeta_2.
                                                  //|    λeta_3.
                                                  //|      IfThenElse(ℤ)
                                                  //|        eta_1
                                                  //|        eta_2
                                                  //|        eta_3)
                                                  //|     True
                                                  //|     (λunit.
                                                  //|        LiteralInt(1))
                                                  //|     (λunit.
                                                  //|        LiteralInt(2));
                                                  //| x"
  "\n" + pretty(aNormalizeTerm(etaExpandPrimitives(test3)))
                                                  //> res7: String = "
                                                  //| a_8 =
                                                  //|   λeta_4.
                                                  //|   λeta_5.
                                                  //|   λeta_6.
                                                  //|     a_5 =
                                                  //|       IfThenElse(ℤ)
                                                  //|         eta_4;
                                                  //|     a_6 =
                                                  //|       a_5
                                                  //|         eta_5;
                                                  //|     a_7 =
                                                  //|       a_6
                                                  //|         eta_6;
                                                  //|     a_7;
                                                  //| a_9 =
                                                  //|   a_8
                                                  //|     True;
                                                  //| a_10 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_11 =
                                                  //|   a_9
                                                  //|     a_10;
                                                  //| a_12 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| a_13 =
                                                  //|   a_11
                                                  //|     a_12;
                                                  //| a_13"
  "\n" + pretty(addCaches(test3))                 //> res8: String = "
                                                  //| a_17 =
                                                  //|   λeta_7.
                                                  //|   λeta_8.
                                                  //|   λeta_9.
                                                  //|     a_14 =
                                                  //|       IfThenElse(ℤ)
                                                  //|         eta_7;
                                                  //|     aTot_10 =
                                                  //|       a_14
                                                  //|         eta_8;
                                                  //|     a_15 =
                                                  //|       Proj1((UnitType → ℤ) → ℤ, UnknownType())
                                                  //|         aTot_10;
                                                  //|     aTot_11 =
                                                  //|       a_15
                                                  //|         eta_9;
                                                  //|     a_16 =
                                                  //|       Proj1(ℤ, UnknownType())
                                                  //|         aTot_11;
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|      λx3lit.
                                                  //|        Pair(ℤ, ProductType(ProductType(ℤ,UnknownType()),ProductType((Un
                                                  //| itType → ℤ) → ℤ,UnknownType())))
                                                  //|          x1lit
                                                  //|          (Pair(ProductType(ℤ,UnknownType()), ProductType((UnitType → �
                                                  //| �) → ℤ,UnknownType()))
                                                  //|             x2lit
                                                  //|             x3lit))
                                                  //|       a_16
                                                  //|       aTot_11
                                                  //|       aTot_10;
                                                  //| aTot_12 =
                                                  //|   a_17
                                                  //|     True;
                                                  //| a_18 =
                                                  //|   Proj1((UnitType → ℤ) → (UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_12;
                                                  //| a_19 =
                                                  //|   λunit.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       LiteralInt(1)
                                                  //|       UnitTerm;
                                                  //| aTot_13 =
                                                  //|   a_18
                                                  //|     a_19;
                                                  //| a_20 =
                                                  //|   Proj1((UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_13;
                                                  //| a_21 =
                                                  //|   λunit.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       LiteralInt(2)
                                                  //|       UnitTerm;
                                                  //| aTot_14 =
                                                  //|   a_20
                                                  //|     a_21;
                                                  //| a_22 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_14;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|  λx5lit.
                                                  //|  λx6lit.
                                                  //|  λx7lit.
                                                  //|    Pair(ℤ, ProductType(ProductType(ℤ,UnknownType()),ProductType(UnitTyp
                                                  //| e → ℤ,ProductType(ProductType((UnitType → ℤ) → ℤ,UnknownType())
                                                  //| ,ProductType(UnitType → ℤ,ProductType(ProductType((UnitType → ℤ) �
                                                  //| � (UnitType → ℤ) → ℤ,UnknownType()),BooleanType → (UnitType → �2038 ��) → (UnitType → ℤ) → ℤ))))))
                                                  //|      x1lit
                                                  //|      (Pair(ProductType(ℤ,UnknownType()), ProductType(UnitType → ℤ,Pro
                                                  //| ductType(ProductType((UnitType → ℤ) → ℤ,UnknownType()),ProductType(
                                                  //| UnitType → ℤ,ProductType(ProductType((UnitType → ℤ) → (UnitType �2038 �� ℤ) → ℤ,UnknownType()),BooleanType → (UnitType → ℤ) → (Unit
                                                  //| Type → ℤ) → ℤ)))))
                                                  //|         x2lit
                                                  //|         (Pair(UnitType → ℤ, ProductType(ProductType((UnitType → ℤ) 
                                                  //| → ℤ,UnknownType()),ProductType(UnitType → ℤ,ProductType(ProductType
                                                  //| ((UnitType → ℤ) → (UnitType → ℤ) → ℤ,UnknownType()),BooleanTy
                                                  //| pe → (UnitType → ℤ) → (UnitType → ℤ) → ℤ))))
                                                  //|            x3lit
                                                  //|            (Pair(ProductType((UnitType → ℤ) → ℤ,UnknownType()), Pro
                                                  //| ductType(UnitType → ℤ,ProductType(ProductType((UnitType → ℤ) → (U
                                                  //| nitType → ℤ) → ℤ,UnknownType()),BooleanType → (UnitType → ℤ) 
                                                  //| → (UnitType → ℤ) → ℤ)))
                                                  //|               x4lit
                                                  //|               (Pair(UnitType → ℤ, ProductType(ProductType((UnitType →
                                                  //|  ℤ) → (UnitType → ℤ) → ℤ,UnknownType()),BooleanType → (UnitTy
                                                  //| pe → ℤ) → (UnitType → ℤ) → ℤ))
                                                  //|                  x5lit
                                                  //|                  (Pair(ProductType((UnitType → ℤ) → (UnitType → ℤ
                                                  //| ) → ℤ,UnknownType()), BooleanType → (UnitType → ℤ) → (UnitType 
                                                  //| → ℤ) → ℤ)
                                                  //|                     x6lit
                                                  //|                     x7lit))))))
                                                  //|   a_22
                                                  //|   aTot_14
                                                  //|   a_21
                                                  //|   aTot_13
                                                  //|   a_19
                                                  //|   aTot_12
                                                  //|   a_17"
  "\n" + pretty(addCaches(test1))                 //> res9: String = "
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
                                                  //|   λx.
                                                  //|     aTot_15 =
                                                  //|       f
                                                  //|         x;
                                                  //|     a_23 =
                                                  //|       Proj1(ℤ, UnknownType())
                                                  //|         aTot_15;
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, ProductType(ℤ,UnknownType()))
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       a_23
                                                  //|       aTot_15;
                                                  //| aTot_16 =
                                                  //|   id
                                                  //|     apply;
                                                  //| a_24 =
                                                  //|   Proj1((ℤ → ℤ) → ℤ → ℤ, UnknownType())
                                                  //|     aTot_16;
                                                  //| aTot_17 =
                                                  //|   a_24
                                                  //|     id_i;
                                                  //| a_25 =
                                                  //|   Proj1(ℤ → ℤ, UnknownType())
                                                  //|     aTot_17;
                                                  //| aTot_18 =
                                                  //|   id_i
                                                  //|     LiteralInt(3);
                                                  //| a_26 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_18;
                                                  //| aTot_19 =
                                                  //|   a_25
                                                  //|     a_26;
                                                  //| a_27 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_19;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|  λx5lit.
                                                  //|  λx6lit.
                                                  //|  λx7lit.
                                                  //|  λx8lit.
                                                  //|    Pair(ℤ, ProductType(ProductType(ℤ,UnknownType()),ProductType(Product
                                                  //| Type(ℤ,UnknownType()),ProductType(ProductType(ℤ → ℤ,UnknownType()),
                                                  //| ProductType(ProductType((ℤ → ℤ) → ℤ → ℤ,UnknownType()),Produc
                                                  //| tType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) 
                                                  //| → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))))))
                                                  //|      x1lit
                                                  //|      (Pair(ProductType(ℤ,UnknownType()), ProductType(ProductType(ℤ,Unkn
                                                  //| ownType()),ProductType(ProductType(ℤ → ℤ,UnknownType()),ProductType(P
                                                  //| roductType((ℤ → ℤ) → ℤ → ℤ,UnknownType()),ProductType((ℤ �
                                                  //| � ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → �
                                                  //| ��) → (ℤ → ℤ) → ℤ → ℤ))))))
                                                  //|         x2lit
                                                  //|         (Pair(ProductType(ℤ,UnknownType()), ProductType(ProductType(ℤ �2072 �� ℤ,UnknownType()),ProductType(ProductType((ℤ → ℤ) → ℤ → ℤ
                                                  //| ,UnknownType()),ProductType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ �
                                                  //| �� ℤ,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))
                                                  //| ))
                                                  //|            x3lit
                                                  //|            (Pair(ProductType(ℤ → ℤ,UnknownType()), ProductType(Produc
                                                  //| tType((ℤ → ℤ) → ℤ → ℤ,UnknownType()),ProductType((ℤ → ℤ
                                                  //| ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → ℤ) �
                                                  //| �� (ℤ → ℤ) → ℤ → ℤ))))
                                                  //|               x4lit
                                                  //|               (Pair(ProductType((ℤ → ℤ) → ℤ → ℤ,UnknownType()
                                                  //| ), ProductType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ 
                                                  //| → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))
                                                  //|                  x5lit
                                                  //|                  (Pair((ℤ → ℤ) → ℤ → ℤ, ProductType(ℤ → �
                                                  //| ��,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ))
                                                  //|                     x6lit
                                                  //|                     (Pair(ℤ → ℤ, ((ℤ → ℤ) → ℤ → ℤ) → 
                                                  //| (ℤ → ℤ) → ℤ → ℤ)
                                                  //|                        x7lit
                                                  //|                        x8lit)))))))
                                                  //|   a_27
                                                  //|   aTot_19
                                                  //|   aTot_18
                                                  //|   aTot_17
                                                  //|   aTot_16
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