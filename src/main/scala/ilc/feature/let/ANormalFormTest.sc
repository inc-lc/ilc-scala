package ilc
package feature
package let

object ANormalFormTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import language._
  val v = new Bacchus with let.ANormalForm with integers.ImplicitSyntaxSugar with inference.LetInference
    //with inference.SyntaxSugar //Or with:
    with inference.LetSyntaxSugar                 //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalForm with ilc.feature.
                                                  //| integers.ImplicitSyntaxSugar with ilc.feature.inference.LetInference with il
                                                  //| c.feature.inference.LetSyntaxSugar = ilc.feature.let.ANormalFormTest$$anonfu
                                                  //| n$main$1$$anon$1@445fc010
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
      }                                           //> test1  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(id,UAbs(x,None
                                                  //| ,UVar(x)),ULet(id_i,UAbs(x,None,UVar(x)),ULet(apply,UAbs(f,None,UAbs(x,None,
                                                  //| UApp(UVar(f),UVar(x)))),UApp(UApp(UApp(UVar(id),UVar(apply)),UVar(id_i)),UMo
                                                  //| nomorphicConstant(LiteralInt(3))))))
/*
(define t2
  '(let ([x (let ([y 20]) y)])
     x))
 */
  val test2 =
    let('x, let('y, 20: Term)('y))('x)            //> test2  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,ULet(y,UMono
                                                  //| morphicConstant(LiteralInt(20)),UVar(y)),UVar(x))
/*
(define t3
  '(let ([x (if #t 1 2)])
     x))
*/
  val test3 =
    let('x, ifThenElse(True, 1, 2): Term)('x)     //> test3  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = ULet(x,UMonomorphic
                                                  //| Constant(App(App(App(IfThenElse(ℤ),True),Abs(Var(unit,UnitType),LiteralInt
                                                  //| (1))),Abs(Var(unit,UnitType),LiteralInt(2)))),UVar(x))
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
                                                  //| apply =
                                                  //|   λf.
                                                  //|   λx.
                                                  //|     f
                                                  //|       x;
                                                  //| id
                                                  //|   apply
                                                  //|   id_i
                                                  //|   LiteralInt(3)
  "\n" + pretty(test1)                            //> res1: String = "
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
                                                  //| id
                                                  //|   apply
                                                  //|   id_i
                                                  //|   LiteralInt(3)"
  "\n" + pretty(normalizeTerm(test1))             //> res2: String = "
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
                                                  //|   id
                                                  //|     apply;
                                                  //| a_2 =
                                                  //|   a_1
                                                  //|     id_i;
                                                  //| a_2
                                                  //|   LiteralInt(3)"
  "\n" + pretty(test2)                            //> res3: String = "
                                                  //| x =
                                                  //|   y =
                                                  //|     LiteralInt(20);
                                                  //|   y;
                                                  //| x"
  "\n" + pretty(normalizeTerm(test2))             //> res4: String = "
                                                  //| y =
                                                  //|   LiteralInt(20);
                                                  //| x =
                                                  //|   y;
                                                  //| x"
  "\n" + pretty(test3)                            //> res5: String = "
                                                  //| x =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True
                                                  //|     (λunit.
                                                  //|        LiteralInt(1))
                                                  //|     (λunit.
                                                  //|        LiteralInt(2));
                                                  //| x"
  "\n" + pretty(normalizeTerm(test3))             //> res6: String = "
                                                  //| a_3 =
                                                  //|   IfThenElse(ℤ)
                                                  //|     True;
                                                  //| a_4 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_5 =
                                                  //|   a_3
                                                  //|     a_4;
                                                  //| a_6 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| x =
                                                  //|   a_5
                                                  //|     a_6;
                                                  //| x"
}