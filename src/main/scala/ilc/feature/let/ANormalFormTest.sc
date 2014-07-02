package ilc
package feature
package let

object ANormalFormTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import language._
  val v = new Bacchus with feature.let.ANormalForm with inference.SyntaxSugar with integers.ImplicitSyntaxSugar
                                                  //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalForm with ilc.feature.
                                                  //| inference.SyntaxSugar with ilc.feature.integers.ImplicitSyntaxSugar = ilc.fe
                                                  //| ature.let.ANormalFormTest$$anonfun$main$1$$anon$1@59b30ac0
  import v._
  sys.props("file.encoding")                      //> res0: String = UTF-8
  //Taken from http://matt.might.net/articles/a-normalization/, but was ill-typed!
  val test1 =
    letS(
      'id -> ('x ->: 'x),
      'id_i -> ('x ->: 'x),
      'apply -> ('f ->: 'x ->: 'f('x))
    ) {
        'id('apply, 'id_i, asUntyped(3: Term))
      }                                           //> test1  : ilc.feature.let.ANormalFormTest.v.UntypedTerm = UApp(UAbs(id,None,U
                                                  //| App(UAbs(id_i,None,UApp(UAbs(apply,None,UApp(UApp(UApp(UVar(id),UVar(apply))
                                                  //| ,UVar(id_i)),UMonomorphicConstant(LiteralInt(3)))),UAbs(f,None,UAbs(x,None,U
                                                  //| App(UVar(f),UVar(x)))))),UAbs(x,None,UVar(x)))),UAbs(x,None,UVar(x)))
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
  val test1T: Term = test1                        //> test1T  : ilc.feature.let.ANormalFormTest.v.Term = App(Abs(Var(id,((ℤ → 
                                                  //| ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ),App(Abs(Var(id_i,�
                                                  //| � → ℤ),App(Abs(Var(apply,(ℤ → ℤ) → ℤ → ℤ),App(App(App(Var(
                                                  //| id,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ),Var(ap
                                                  //| ply,(ℤ → ℤ) → ℤ → ℤ)),Var(id_i,ℤ → ℤ)),LiteralInt(3))),A
                                                  //| bs(Var(f,ℤ → ℤ),Abs(Var(x,ℤ),App(Var(f,ℤ → ℤ),Var(x,ℤ)))))),
                                                  //| Abs(Var(x,ℤ),Var(x,ℤ)))),Abs(Var(x,(ℤ → ℤ) → ℤ → ℤ),Var(x,
                                                  //| (ℤ → ℤ) → ℤ → ℤ)))
  pretty(test1T)                                  //> res1: String = (λid.
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
  "\n" + pretty(normalizeTerm(test1T))            //> res2: String = "
                                                  //| Av_7 =
                                                  //|   λid.
                                                  //|     Av_5 =
                                                  //|       λid_i.
                                                  //|         Av_3 =
                                                  //|           λapply.
                                                  //|             Av_1 =
                                                  //|               id
                                                  //|                 apply;
                                                  //|             Av_2 =
                                                  //|               Av_1
                                                  //|                 id_i;
                                                  //|             Av_2
                                                  //|               LiteralInt(3);
                                                  //|         Av_4 =
                                                  //|           λf.
                                                  //|           λx.
                                                  //|             f
                                                  //|               x;
                                                  //|         Av_3
                                                  //|           Av_4;
                                                  //|     Av_6 =
                                                  //|       λx.
                                                  //|         x;
                                                  //|     Av_5
                                                  //|       Av_6;
                                                  //| Av_8 =
                                                  //|   λx.
                                                  //|     x;
                                                  //| Av_7
                                                  //|   Av_8"
}