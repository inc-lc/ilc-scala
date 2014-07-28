package ilc
package feature
package let

object ANormalFormTest extends Instantiations {
  val v = buildBacchusWithLetSystem(true, true, true)
                                                  //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalFormAdapter with ilc.f
                                                  //| eature.integers.ImplicitSyntaxSugar with ilc.feature.integers.Evaluation wit
                                                  //| h ilc.feature.let.Evaluation with ilc.feature.let.Pretty with ilc.feature.in
                                                  //| ference.LetInference with ilc.feature.let.BetaReduction with ilc.feature.inf
                                                  //| erence.LetSyntaxSugar with ilc.feature.inference.InferenceTestHelper{val aNo
                                                  //| rmalizer: ilc.feature.let.ANormalFormStateful{val mySyntax: ilc.feature.let.
                                                  //| Instantiations.<refinement>.type}} = ilc.feature.let.Instantiations$$anon$1@
                                                  //| 2cfd88b5
  import v._

  //Taken from http://matt.might.net/articles/a-normalization/, but was ill-typed!
/*
(define t1
 '(let ((id (λ (x) x)))
    (let ((apply (λ (f x) (f x))))
      ((id apply) (id 3)))))
  */
  val test1 =
    letS(
      'id -> ('x ->: 'x),
      'id_i -> ('x ->: 'x),
      'id_i2 -> ('x ->: 'x),
      'apply -> ('f ->: 'x ->: 'f('x))
    ) {
        'id('apply, 'id_i, 'id_i2(3: Term))
      }                                           //> test1  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(id,UAbs(x,Non
                                                  //| e,UVar(x)),ULet(id_i,UAbs(x,None,UVar(x)),ULet(id_i2,UAbs(x,None,UVar(x)),U
                                                  //| Let(apply,UAbs(f,None,UAbs(x,None,UApp(UVar(f),UVar(x)))),UApp(UApp(UApp(UV
                                                  //| ar(id),UVar(apply)),UVar(id_i)),UApp(UVar(id_i2),UMonomorphicConstant(Liter
                                                  //| alInt(3))))))))
/*
(define t2
  '(let ([x (let ([y 20]) y)])
     x))
 */
  val test2 =
    let('x, let('y, 20: Term)('y))('x)            //> test2  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,ULet(y,UMon
                                                  //| omorphicConstant(LiteralInt(20)),UVar(y)),UVar(x))
  val test2Bis =
    let('x, let('y, (PlusInt ! (20: Term) ! (30: Term)): Term)('y))('x)
                                                  //> test2Bis  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,ULet(y,U
                                                  //| MonomorphicConstant(App(App(PlusInt,LiteralInt(20)),LiteralInt(30))),UVar(y
                                                  //| )),UVar(x))
/*
(define t3
  '(let ([x (if #t 1 2)])
     x))
*/
  val test3 =
    let('x, ifThenElse(True, 1, 2): Term)('x)     //> test3  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,UMonomorphi
                                                  //| cConstant(App(App(App(IfThenElse(ℤ),True),Abs(Var(unit,UnitType),LiteralI
                                                  //| nt(1))),Abs(Var(unit,UnitType),LiteralInt(2)))),UVar(x))
  try {
    println(pretty(test1))
  } catch { case e: inference.Inference#UnificationFailure =>
    println(e.details)
  }                                               //> id =
                                                  //|   λx.
                                                  //|     x;
                                                  //| id_i =
                                                  //|   λx.
                                                  //|     x;
                                                  //| id_i2lit =
                                                  //|   λx.
                                                  //|     x;
                                                  //| apply =
                                                  //|   λf.
                                                  //|   λx.
                                                  //|     f
                                                  //|       x;
                                                  //| id
                                                  //|   apply
                                                  //|   id_i
                                                  //|   (id_i2lit
                                                  //|      LiteralInt(3))
  "\n" + pretty(test1)                            //> res0: String = "
                                                  //| id =
                                                  //|   λx.
                                                  //|     x;
                                                  //| id_i =
                                                  //|   λx.
                                                  //|     x;
                                                  //| id_i2lit =
                                                  //|   λx.
                                                  //|     x;
                                                  //| apply =
                                                  //|   λf.
                                                  //|   λx.
                                                  //|     f
                                                  //|       x;
                                                  //| id
                                                  //|   apply
                                                  //|   id_i
                                                  //|   (id_i2lit
                                                  //|      LiteralInt(3))"
  "\n" + pretty(aNormalizeTerm(test1))            //> res1: String = "
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
                                                  //| a_2 =
                                                  //|   id_i
                                                  //|     LiteralInt(3);
                                                  //| a_3 =
                                                  //|   id
                                                  //|     apply
                                                  //|     id_i
                                                  //|     a_2;
                                                  //| a_3"
  "\n" + pretty(normalize(aNormalizeTerm(test1))) //> res2: String = "
                                                  //| LiteralInt(3)"
  "\n" + pretty(aNormalizeTerm(normalize(test1))) //> res3: String = "
                                                  //| LiteralInt(3)"
  "\n" + pretty(test2)                            //> res4: String = "
                                                  //| x =
                                                  //|   y =
                                                  //|     LiteralInt(20);
                                                  //|   y;
                                                  //| x"
  "\n" + pretty(aNormalizeTerm(test2))            //> res5: String = "
                                                  //| LiteralInt(20)"
  "\n" + pretty(normalize(aNormalizeTerm(test2))) //> res6: String = "
                                                  //| LiteralInt(20)"
  "\n" + pretty(aNormalizeTerm(normalize(test2))) //> res7: String = "
                                                  //| LiteralInt(20)"
  "\n" + pretty(test2Bis)                         //> res8: String = "
                                                  //| x =
                                                  //|   y =
                                                  //|     PlusInt
                                                  //|       LiteralInt(20)
                                                  //|       LiteralInt(30);
                                                  //|   y;
                                                  //| x"
  "\n" + pretty(aNormalizeTerm(test2Bis))         //> res9: String = "
                                                  //| a_7 =
                                                  //|   PlusInt
                                                  //|     LiteralInt(20)
                                                  //|     LiteralInt(30);
                                                  //| a_7"
  "\n" + pretty(normalize(aNormalizeTerm(test2Bis)))
                                                  //> res10: String = "
                                                  //| PlusInt
                                                  //|   LiteralInt(20)
                                                  //|   LiteralInt(30)"
  "\n" + pretty(aNormalizeTerm(normalize(test2Bis)))
                                                  //> res11: String = "
                                                  //| a_9 =
                                                  //|   PlusInt
                                                  //|     LiteralInt(20)
                                                  //|     LiteralInt(30);
                                                  //| a_9"
  "\n" + pretty(test3)                            //> res12: String = "
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     (λunit.
                                                  //|        LiteralInt(1))
                                                  //|     (λunit.
                                                  //|        LiteralInt(2));
                                                  //| x"
  "\n" + pretty(aNormalizeTerm(test3))            //> res13: String = "
                                                  //| a_10 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_11 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| a_12 =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     a_10
                                                  //|     a_11;
                                                  //| a_12"
  "\n" + pretty(aNormalizeTerm(normalize(test3))) //> res14: String = "
                                                  //| a_13 =
                                                  //|   λunit_1.
                                                  //|     LiteralInt(1);
                                                  //| a_14 =
                                                  //|   λunit_2.
                                                  //|     LiteralInt(2);
                                                  //| a_15 =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     a_13
                                                  //|     a_14;
                                                  //| a_15"
}