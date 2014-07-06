package ilc
package feature
package let

object ANormalFormTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import language._
  val v = new Bacchus with let.ANormalFormAdapter with integers.ImplicitSyntaxSugar with inference.LetInference
    with BetaReduction with Pretty// with AddCaches
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
                                                  //| teful{val mySyntax: ilc.feature.let.ANormalFormTest.<refinement>.type}} = il
                                                  //| c.feature.let.ANormalFormTest$$anonfun$main$1$$anon$1@7b1c661c
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
    let('x, let('y, (PlusInt ! (20: Term) ! (30: Term)): Term)('y))('x)
                                                  //> test2  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,ULet(y,UMon
                                                  //| omorphicConstant(App(App(PlusInt,LiteralInt(20)),LiteralInt(30))),UVar(y)),
                                                  //| UVar(x))
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
                                                  //|   id_i2lit
                                                  //|     LiteralInt(3);
                                                  //| a_3 =
                                                  //|   id
                                                  //|     apply
                                                  //|     id_i
                                                  //|     a_2;
                                                  //| a_3"
  "\n" + pretty(normalize(aNormalizeTerm(test1))) //> ilc.feature.base.TypeError: Type error: Variable id_i2lit not available in 
                                                  //| context Map(id -> FunVal(<function1>,Var(x,(ℤ → ℤ) → ℤ → ℤ),t
                                                  //| rue), id_i -> FunVal(<function1>,Var(x,ℤ),true), apply -> FunVal(<functio
                                                  //| n1>,Var(f,ℤ → ℤ),false)), ill-scoped term
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$.eval(BetaReduction.scala:196
                                                  //| )
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$.eval(BetaReduction.scala:192
                                                  //| )
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$.eval(BetaReduction.scala:168
                                                  //| )
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$$anonfun$eval$1.apply(BetaRed
                                                  //| uction.scala:166)
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$$anonfun$eval$1.apply(BetaRed
                                                  //| uction.scala:166)
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$.ilc$feature$let$BetaReductio
                                                  //| n$Normalize$$findFun$1(BetaReduction.scala:176)
                                                  //| 	at ilc.feature.let.BetaReduction$Normalize$.eval(BetaReduction.scala:192
                                                  //| )
                                                  //| 	at ilc.feature.let.BetaReduction$Normali
                                                  //| Output exceeds cutoff limit.
  "\n" + pretty(aNormalizeTerm(normalize(test1)))
  "\n" + pretty(test2)
  "\n" + pretty(aNormalizeTerm(test2))
  "\n" + pretty(normalize(aNormalizeTerm(test2)))
  "\n" + pretty(aNormalizeTerm(normalize(test2)))
  "\n" + pretty(test3)
  "\n" + pretty(aNormalizeTerm(test3))
  "\n" + pretty(aNormalizeTerm(normalize(test3)))
  //"\n" + pretty(addCaches(test3))
  //"\n" + pretty(addCaches(test1))
  //}
  //tests(v)
}