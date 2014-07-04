package ilc
package feature
package let

object ANormalFormTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import language._
  val v = new Bacchus with let.ANormalFormStateful with integers.ImplicitSyntaxSugar with inference.LetInference
    with BetaReduction with Pretty with AddCaches
    //with inference.SyntaxSugar //Or with:
    with inference.LetSyntaxSugar                 //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalFormStateful with ilc.
                                                  //| feature.integers.ImplicitSyntaxSugar with ilc.feature.inference.LetInference
                                                  //|  with ilc.feature.let.BetaReduction with ilc.feature.let.Pretty with ilc.fea
                                                  //| ture.let.AddCaches with ilc.feature.inference.LetSyntaxSugar = ilc.feature.l
                                                  //| et.ANormalFormTest$$anonfun$main$1$$anon$1@7a2520fa
    //Both work, but the output is different.

  //def tests(v: Bacchus with feature.let.ANormalFormStateful with integers.ImplicitSyntaxSugar with inference.LetInference
    //with BetaReduction) {
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
      }                                           //> test1  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(id,UAbs(x,None
                                                  //| ,UVar(x)),ULet(id_i,UAbs(x,None,UVar(x)),ULet(id_i2,UAbs(x,None,UVar(x)),ULe
                                                  //| t(apply,UAbs(f,None,UAbs(x,None,UApp(UVar(f),UVar(x)))),UApp(UApp(UApp(UVar(
                                                  //| id),UVar(apply)),UVar(id_i)),UApp(UVar(id_i2),UMonomorphicConstant(LiteralIn
                                                  //| t(3))))))))
/*
(define t2
  '(let ([x (let ([y 20]) y)])
     x))
 */
  val test2 =
    let('x, let('y, 20: Term)('y))('x)            //> test2  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,ULet(y,UMon
                                                  //| omorphicConstant(LiteralInt(20)),UVar(y)),UVar(x))
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
                                                  //|     f
                                                  //|       x;
                                                  //| a_1 =
                                                  //|   id_i
                                                  //|     LiteralInt(3);
                                                  //| id
                                                  //|   apply
                                                  //|   id_i
                                                  //|   a_1"
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
  "\n" + pretty(test3)                            //> res8: String = "
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     (λunit.
                                                  //|        LiteralInt(1))
                                                  //|     (λunit.
                                                  //|        LiteralInt(2));
                                                  //| x"
  "\n" + pretty(aNormalizeTerm(test3))            //> res9: String = "
                                                  //| a_3 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_4 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     a_3
                                                  //|     a_4;
                                                  //| x"
  "\n" + pretty(aNormalizeTerm(normalize(test3))) //> res10: String = "
                                                  //| a_5 =
                                                  //|   λunit_1.
                                                  //|     LiteralInt(1);
                                                  //| a_6 =
                                                  //|   λunit_2.
                                                  //|     LiteralInt(2);
                                                  //| IfThenElse(ℤ)
                                                  //|   True
                                                  //|   a_5
                                                  //|   a_6"
  "\n" + pretty(addCaches(test3))                 //> res11: String = "
                                                  //| a_7 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_8 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     a_7
                                                  //|     a_8;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|    Pair(ℤ, ProductType(ℤ,ProductType(UnitType → ℤ,UnitType → ℤ)
                                                  //| ))
                                                  //|      x1lit
                                                  //|      (Pair(ℤ, ProductType(UnitType → ℤ,UnitType → ℤ))
                                                  //|         x2lit
                                                  //|         (Pair(UnitType → ℤ, UnitType → ℤ)
                                                  //|            x3lit
                                                  //|            x4lit)))
                                                  //|   x
                                                  //|   x
                                                  //|   a_8
                                                  //|   a_7"
  //}
  //tests(v)
}