package ilc
package feature
package let

object CPSPlay extends Instantiations {

  val v = buildBacchusWithLetSystem(true, true, true)
                                                  //> v  : ilc.language.Bacchus with ilc.feature.let.ANormalFormAdapter with ilc.f
                                                  //| eature.integers.ImplicitSyntaxSugar with ilc.feature.integers.Evaluation wit
                                                  //| h ilc.feature.let.Evaluation with ilc.feature.let.Pretty with ilc.feature.le
                                                  //| t.CPS with ilc.feature.cbpv.CBPVToCPS with ilc.feature.inference.LetInferenc
                                                  //| e with ilc.feature.let.BetaReduction with ilc.feature.inference.LetSyntaxSug
                                                  //| ar with ilc.feature.inference.InferenceTestHelper with ilc.feature.let.ShowT
                                                  //| erms{val aNormalizer: ilc.feature.let.ANormalFormStateful{val mySyntax: ilc.
                                                  //| feature.let.Instantiations.<refinement>.type}} = ilc.feature.let.Instantiati
                                                  //| ons$$anon$1@731a74c

  import v._
  def testCPS(t: Term) = {
    println(pretty(t))
    val tau = t.getType
    println(toCPST(tau))
    println(cbvTypeToCPS(tau))
    val untypedCPS = toCPSU(t)
    println(untypedCPS)
    println(pretty(untypedCPS))
    println(pretty(toCPS(t)))
  }                                               //> testCPS: (t: ilc.feature.let.CPSPlay.v.Term)Unit
  testCPS('x ->: 'x)                              //> λx.
                                                  //|   x
                                                  //| ((T1 -> (T1 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT
                                                  //| (T1) x (T1 -> AnswerT) -> AnswerT
                                                  //| UAbs(k_1,None,UApp(UVar(k_1),UAbs(x,None,UAbs(k_2,None,UApp(UVar(k_2),UVar(x
                                                  //| ))))))
                                                  //| λk_1lit.
                                                  //|   k_1lit
                                                  //|     (λx.
                                                  //|      λk_2lit.
                                                  //|        k_2lit
                                                  //|          x)
                                                  //| λk_3.
                                                  //|   k_3
                                                  //|     (λx.
                                                  //|      λk_4.
                                                  //|        k_4
                                                  //|          x)
  testCPS(Var("x", freshTypeVariable))            //> x
                                                  //| (T7 -> AnswerT) -> AnswerT
                                                  //| T7
                                                  //| UAbs(k_5,None,UApp(UVar(k_5),UVar(x)))
                                                  //| java.lang.RuntimeException: Unbound variable UVar(x)
                                                  //| 	at scala.sys.package$.error(package.scala:27)
                                                  //| 	at ilc.feature.inference.Inference$class.collectConstraints(Inference.sc
                                                  //| ala:99)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.ilc$feature$inference$LetInfer
                                                  //| ence$$super$collectConstraints(Instantiations.scala:14)
                                                  //| 	at ilc.feature.inference.LetInference$class.collectConstraints(Inference
                                                  //| .scala:207)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.collectConstraints(Instantiati
                                                  //| ons.scala:14)
                                                  //| 	at ilc.feature.inference.Inference$class.collectConstraints(Inference.sc
                                                  //| ala:107)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.ilc$feature$inference$LetInfer
                                                  //| ence$$super$collectConstraints(Instantiations.scala:14)
                                                  //| 	at ilc.feature.inference.LetInference$class.collectConstraints(Inference
                                                  //| .scala:207)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.collectConstraints(Instantiati
                                                  //| ons.scala:14)
                                                  //| 	at ilc.feature.inference.Inference$class.collectConstraints(Inference.sc
                                                  //| ala:103)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.ilc$feature$inference$LetInfer
                                                  //| ence$$super$collectConstraints(Instantiations.scala:14)
                                                  //| 	at ilc.feature.inference.LetInference$class.collectConstraints(Inference
                                                  //| .scala:207)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.collectConstraints(Instantiati
                                                  //| ons.scala:14)
                                                  //| 	at ilc.feature.inference.Inference$class.inferType(Inference.scala:191)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.inferType(Instantiations.scala
                                                  //| :14)
                                                  //| 	at ilc.feature.inference.PrettySyntax$class.untypedTermToTerm(PrettySynt
                                                  //| ax.scala:7)
                                                  //| 	at ilc.feature.let.Instantiations$$anon$1.untypedTermToTerm(Instantiatio
                                                  //| ns.scala:14)
                                                  //| 	at ilc.feature.let.CPSPlay$$anonfun$main$1.testCPS$1(ilc.feature.let.CPS
                                                  //| Play.scala:17)
                                                  //| 	at ilc.feature.let.CPSPlay$$anonfun$main$1.apply$mcV$sp(ilc.feature.let.
                                                  //| CPSPlay.scala:21)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at ilc.feature.let.CPSPlay$.main(ilc.feature.let.CPSPlay.scala:5)
                                                  //| 	at ilc.feature.let.CPSPlay.main(ilc.feature.let.CPSPlay.scala)
  val tst0 = asTerm('x ->: 'x)
  println(pretty(tst0))
  
  tst0.getType
  println(toCPST(tst0.getType))
  println(cbvTypeToCPS(tst0.getType))
  
  println(pretty(toCPS(tst0)))
  println(pretty(toCPSU(tst0)))
 
  val tst1 = asTerm('f ->: 'x ->: 'f('x))
  println(pretty(tst1))

  tst1.getType
  println(toCPST(tst1.getType))
  println(cbvTypeToCPS(tst1.getType))
  val tst1Cps = asTerm(toCPSU(tst1))
  println(pretty(tst1Cps))
  println(pretty(toCPS(tst1)))
  
  val tst = asTerm('f ->: 'x ->: 'y ->: 'f('y)('x))
  pretty(tst)
  toCPS(tst)
}