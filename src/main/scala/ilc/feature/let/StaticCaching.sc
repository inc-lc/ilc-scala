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
                                                  //| 1@39c3b1dd
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
                                                  //| n$2@5bbb8077
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
  "\n" + pretty(addCaches(test3))                 //> scala.MatchError: App(IfThenElse(ℤ),Var(eta_7,BooleanType)) (of class ilc
                                                  //| .feature.functions.Syntax$App)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendLetRule$1.applyOrElse(ANor
                                                  //| malForm.scala:407)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendLetRule$1.applyOrElse(ANor
                                                  //| malForm.scala:404)
                                                  //| 	at scala.PartialFunction$OrElse.apply(PartialFunction.scala:162)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendAbsRule$1.applyOrElse(ANor
                                                  //| malForm.scala:384)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendAbsRule$1.applyOrElse(ANor
                                                  //| malForm.scala:384)
                                                  //| 	at scala.PartialFunction$OrElse.apply(PartialFunction.scala:162)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendAbsRule$1.applyOrElse(ANor
                                                  //| malForm.scala:386)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendAbsRule$1.applyOrElse(ANor
                                                  //| malForm.scala:384)
                                                  //| 	at scala.PartialFunction$OrElse.apply(PartialFunction.scala:162)
                                                  //| 	at ilc.feature.let.AddCaches2$$anonfun$descendAb
                                                  //| Output exceeds cutoff limit.
  "\n" + pretty(addCaches(test1))
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