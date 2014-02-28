package ilc.feature.inference

import scala.language.implicitConversions

trait Pretty extends Inference {
  implicit def polymorphicConstantToUPolymorphicConstant(x: PolymorphicConstant): UntypedTerm = UPolymorphicConstant(x)
  implicit def termToUTerm(x: Term): UntypedTerm = UTerm(x)
  implicit def symbolToUVar(x: Symbol): UVar = UVar(x.name)

  implicit def pairSyntaxForAbstraction1(kv: (UVar, UntypedTerm)): UntypedTerm = UAbs(kv._1, kv._2)
  implicit def pairSyntaxForAbstraction2(kv: (Symbol, UntypedTerm)): UntypedTerm = UAbs(UVar(kv._1.name), kv._2)
  implicit def pairSyntaxForAbstraction3(kv: (Symbol, Symbol)): UntypedTerm = UAbs(UVar(kv._1.name), UVar(kv._2.name))
}
