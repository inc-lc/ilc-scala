package ilc
package feature
package let

object ANormalFormTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import language._
  val v = new Bacchus with let.ANormalForm with integers.ImplicitSyntaxSugar with inference.LetInference
    with inference.SyntaxSugar //Or with:         //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalForm with ilc.feature.
                                                  //| integers.ImplicitSyntaxSugar with ilc.feature.inference.LetInference with il
                                                  //| c.feature.inference.SyntaxSugar = ilc.feature.let.ANormalFormTest$$anonfun$m
                                                  //| ain$1$$anon$1@505e9087
    //with inference.LetSyntaxSugar
    //Both work, but the output is different.
  import v._
  sys.props("file.encoding")                      //> res0: String = UTF-8
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
      'apply -> ('f ->: 'x ->: 'f('x))
    ) {
        'id('apply, 'id_i, 3: Term)
      }                                           //> test1  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = UApp(UAbs(id,None,U
                                                  //| App(UAbs(id_i,None,UApp(UAbs(apply,None,UApp(UApp(UApp(UVar(id),UVar(apply))
                                                  //| ,UVar(id_i)),UMonomorphicConstant(LiteralInt(3)))),UAbs(f,None,UAbs(x,None,U
                                                  //| App(UVar(f),UVar(x)))))),UAbs(x,None,UVar(x)))),UAbs(x,None,UVar(x)))
/*
(define t2
  '(let ([x (let ([y 20]) y)])
     x))
 */
  val test2 =
    let('x, let('y, 20: Term)('y))('x)            //> test2  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = UApp(UAbs(x,None,UV
                                                  //| ar(x)),UApp(UAbs(y,None,UVar(y)),UMonomorphicConstant(LiteralInt(20))))
/*
(define t3
  '(let ([x (if #t 1 2)])
     x))
*/
  val test3 =
    let('x, ifThenElse(True, 1, 2): Term)('x)     //> test3  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = UApp(UAbs(x,None,UV
                                                  //| ar(x)),UMonomorphicConstant(App(App(App(IfThenElse(ℤ),True),Abs(Var(unit,U
                                                  //| nitType),LiteralInt(1))),Abs(Var(unit,UnitType),LiteralInt(2)))))
  try {
    println(pretty(test1))
  } catch { case e: inference.Inference#UnificationFailure =>
    println(e.details)
  }                                               //> (λid.
                                                  //|    (λid_i.
                                                  //|       (λapply.
                                                  //|          id
                                                  //|            apply
                                                  //|            id_i
                                                  //|            LiteralInt(3))
                                                  //|         (λf.
                                                  //|          λx.
                                                  //|            f
                                                  //|              x))
                                                  //|      (λx.
                                                  //|         x))
                                                  //|   (λx.
                                                  //|      x)
  "\n" + pretty(test1)                            //> res1: String = "
                                                  //| (λid.
                                                  //|    (λid_i.
                                                  //|       (λapply.
                                                  //|          id
                                                  //|            apply
                                                  //|            id_i
                                                  //|            LiteralInt(3))
                                                  //|         (λf.
                                                  //|          λx.
                                                  //|            f
                                                  //|              x))
                                                  //|      (λx.
                                                  //|         x))
                                                  //|   (λx.
                                                  //|      x)"
  "\n" + pretty(normalizeTerm(test1))             //> res2: String = "
                                                  //| a_7 =
                                                  //|   λid.
                                                  //|     a_5 =
                                                  //|       λid_i.
                                                  //|         a_3 =
                                                  //|           λapply.
                                                  //|             a_1 =
                                                  //|               id
                                                  //|                 apply;
                                                  //|             a_2 =
                                                  //|               a_1
                                                  //|                 id_i;
                                                  //|             a_2
                                                  //|               LiteralInt(3);
                                                  //|         a_4 =
                                                  //|           λf.
                                                  //|           λx.
                                                  //|             f
                                                  //|               x;
                                                  //|         a_3
                                                  //|           a_4;
                                                  //|     a_6 =
                                                  //|       λx.
                                                  //|         x;
                                                  //|     a_5
                                                  //|       a_6;
                                                  //| a_8 =
                                                  //|   λx.
                                                  //|     x;
                                                  //| a_7
                                                  //|   a_8"
  "\n" + pretty(test2)                            //> res3: String = "
                                                  //| (λx.
                                                  //|    x)
                                                  //|   ((λy.
                                                  //|       y)
                                                  //|      LiteralInt(20))"
  "\n" + pretty(normalizeTerm(test2))             //> res4: String = "
                                                  //| a_9 =
                                                  //|   λx.
                                                  //|     x;
                                                  //| a_10 =
                                                  //|   λy.
                                                  //|     y;
                                                  //| a_11 =
                                                  //|   a_10
                                                  //|     LiteralInt(20);
                                                  //| a_9
                                                  //|   a_11"
  "\n" + pretty(test3)                            //> res5: String = "
                                                  //| (λx.
                                                  //|    x)
                                                  //|   (IfThenElse(ℤ)
                                                  //|      True
                                                  //|      (λunit.
                                                  //|         LiteralInt(1))
                                                  //|      (λunit.
                                                  //|         LiteralInt(2)))"
  "\n" + pretty(normalizeTerm(test3))             //> res6: String = "
                                                  //| a_12 =
                                                  //|   λx.
                                                  //|     x;
                                                  //| a_13 =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True;
                                                  //| a_14 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_15 =
                                                  //|   a_13
                                                  //|     a_14;
                                                  //| a_16 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| a_17 =
                                                  //|   a_15
                                                  //|     a_16;
                                                  //| a_12
                                                  //|   a_17"
}