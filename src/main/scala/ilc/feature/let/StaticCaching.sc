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
                                                  //| feature.let.StaticCaching$$anonfun$main$1$$anon$1@6eb1054b
  val v1 = new AddCaches {
    val mySyntax: v.type = v
  }                                               //> v1  : ilc.feature.let.AddCaches{val mySyntax: ilc.feature.let.StaticCaching.
                                                  //| <refinement>.type} = ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2@7
                                                  //| 1471ecf

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
  "\n" + pretty(addCaches(test3))                 //> java.lang.RuntimeException: 1-tuples are not supported
                                                  //| 	at scala.sys.package$.error(package.scala:27)
                                                  //| 	at ilc.feature.products.SyntaxSugar$class.tuple(Syntax.scala:51)
                                                  //| 	at ilc.language.Bacchus.tuple(Bacchus.scala:6)
                                                  //| 	at ilc.feature.let.AddCaches$class.go$1(ANormalForm.scala:223)
                                                  //| 	at ilc.feature.let.AddCaches$class.aNormalizeTerm(ANormalForm.scala:229)
                                                  //| 
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalizeTerm(
                                                  //| ilc.feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalize(ANormalForm.scal
                                                  //| a:122)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalize(ilc.
                                                  //| feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalizeName(ANormalForm.
                                                  //| scala:150)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalizeName(
                                                  //| ilc.feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$$anonfun$aNormalize$3.apply(ANorm
                                                  //| alForm.scala:140)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$$anonfun$aNormalize$3.apply(ANorm
                                                  //| alForm.scala:140)
                                                  //| 	at scala.collection.immutable.List.map(List.scala:278)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalize(ANormalForm.scal
                                                  //| a:140)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalize(ilc.
                                                  //| feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalizeName(ANormalForm.
                                                  //| scala:150)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalizeName(
                                                  //| ilc.feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$$anonfun$aNormalize$3.apply(ANorm
                                                  //| alForm.scala:140)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$$anonfun$aNormalize$3.apply(ANorm
                                                  //| alForm.scala:140)
                                                  //| 	at scala.collection.immutable.List.map(List.scala:274)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalize(ANormalForm.scal
                                                  //| a:140)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalize(ilc.
                                                  //| feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalizeName(ANormalForm.
                                                  //| scala:150)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalizeName(
                                                  //| ilc.feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.ANormalFormStateful$class.aNormalize(ANormalForm.scal
                                                  //| a:142)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalize(ilc.
                                                  //| feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.AddCaches$class.aNormalizeTerm(ANormalForm.scala:206)
                                                  //| 
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.aNormalizeTerm(
                                                  //| ilc.feature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.AddCaches$class.addCaches(ANormalForm.scala:200)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1$$anon$2.addCaches(ilc.f
                                                  //| eature.let.StaticCaching.scala:17)
                                                  //| 	at ilc.feature.let.StaticCaching$$anonfun$main$1.apply$mcV$sp(ilc.featur
                                                  //| e.let.StaticCaching.scala:35)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at ilc.feature.let.StaticCaching$.main(ilc.feature.let.StaticCaching.sca
                                                  //| la:7)
                                                  //| 	at ilc.feature.let.StaticCaching.main(ilc.feature.let.StaticCaching.scal
                                                  //| a)
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