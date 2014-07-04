package ilc
package feature
package let

import language._

object StaticCaching {
  val v = new Bacchus with let.ANormalFormAdapter with integers.ImplicitSyntaxSugar with inference.LetInference
    with BetaReduction with Pretty
    //with inference.SyntaxSugar //Or with:
    with inference.LetSyntaxSugar {
      outer =>
      val aNormalizer = new feature.let.ANormalFormStateful {
        val mySyntax: outer.type = outer
      }
    }                                             //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalFormAdapter with ilc.f
                                                  //| eature.integers.ImplicitSyntaxSugar with ilc.feature.inference.LetInference 
                                                  //| with ilc.feature.let.BetaReduction with ilc.feature.let.Pretty with ilc.feat
                                                  //| ure.inference.LetSyntaxSugar{val aNormalizer: ilc.feature.let.ANormalFormSta
                                                  //| teful{val mySyntax: ilc.feature.let.StaticCaching.<refinement>.type}} = ilc.
                                                  //| feature.let.StaticCaching$$anonfun$main$1$$anon$1@460ee450
  val v1 = new AddCaches {
    val mySyntax: v.type = v
  }                                               //> v1  : ilc.feature.let.AddCaches{val mySyntax: ilc.feature.let.StaticCaching.
                                                  //| <refinement>.type} = ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2@5
                                                  //| a254de0

  import v._
  import v1.addCaches
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
  "\n" + pretty(addCaches(test1))                 //> res0: String = "
                                                  //| id =
                                                  //|   λx.
                                                  //|     x;
                                                  //| id_i =
                                                  //|   λx.
                                                  //|     x;
                                                  //| apply =
                                                  //|   λf.
                                                  //|   λx.
                                                  //|     a_1 =
                                                  //|       f
                                                  //|         x;
                                                  //|     a_1;
                                                  //| aTot_2 =
                                                  //|   id
                                                  //|     apply;
                                                  //| a_2 =
                                                  //|   Proj1((ℤ → ℤ) → ℤ → ℤ, UnknownType())
                                                  //|     aTot_2;
                                                  //| aTot_3 =
                                                  //|   a_2
                                                  //|     id_i;
                                                  //| a_3 =
                                                  //|   Proj1(ℤ → ℤ, UnknownType())
                                                  //|     aTot_3;
                                                  //| aTot_4 =
                                                  //|   id_i
                                                  //|     LiteralInt(3);
                                                  //| a_4 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_4;
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
                                                  //|  λx7lit.
                                                  //|  λx8lit.
                                                  //|    Pair(ℤ, ProductType(ℤ,ProductType(ℤ,ProductType(ℤ → ℤ,Product
                                                  //| Type((ℤ → ℤ) → ℤ → ℤ,ProductType((ℤ → ℤ) → ℤ → ℤ
                                                  //| ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) �855 �� ℤ → ℤ)))))))
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
                                                  //|   aTot_5
                                                  //|   aTot_5
                                                  //|   aTot_4
                                                  //|   aTot_3
                                                  //|   aTot_2
                                                  //|   applyTot
                                                  //|   id_iTot
                                                  //|   idTot"
  "\n" + pretty(addCaches(test3))                 //> res1: String = "
                                                  //| aTot_6 =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True;
                                                  //| a_6 =
                                                  //|   Proj1((UnitType → ℤ) → (UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_6;
                                                  //| a_7 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| aTot_8 =
                                                  //|   a_6
                                                  //|     a_7;
                                                  //| a_8 =
                                                  //|   Proj1((UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_8;
                                                  //| a_9 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
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
                                                  //|    Pair(ℤ, ProductType(ℤ,ProductType(UnitType → ℤ,ProductType((UnitT
                                                  //| ype → ℤ) → ℤ,ProductType(UnitType → ℤ,(UnitType → ℤ) → (Un
                                                  //| itType → ℤ) → ℤ)))))
                                                  //|      x1lit
                                                  //|      (Pair(ℤ, ProductType(UnitType → ℤ,ProductType((UnitType → ℤ) 
                                                  //| → ℤ,ProductType(UnitType → ℤ,(UnitType → ℤ) → (UnitType → �
                                                  //| �) → ℤ))))
                                                  //|         x2lit
                                                  //|         (Pair(UnitType → ℤ, ProductType((UnitType → ℤ) → ℤ,Produ
                                                  //| ctType(UnitType → ℤ,(UnitType → ℤ) → (UnitType → ℤ) → ℤ)))
                                                  //| 
                                                  //|            x3lit
                                                  //|            (Pair((UnitType → ℤ) → ℤ, ProductType(UnitType → ℤ,(U
                                                  //| nitType → ℤ) → (UnitType → ℤ) → ℤ))
                                                  //|               x4lit
                                                  //|               (Pair(UnitType → ℤ, (UnitType → ℤ) → (UnitType → �
                                                  //| ��) → ℤ)
                                                  //|                  x5lit
                                                  //|                  x6lit)))))
                                                  //|   aTot_10
                                                  //|   aTot_10
                                                  //|   aTot_9
                                                  //|   aTot_8
                                                  //|   aTot_7
                                                  //|   aTot_6"
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