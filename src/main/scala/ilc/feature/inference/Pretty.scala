package ilc.feature.inference

import scala.language.implicitConversions

trait Pretty extends Inference {
  implicit def polymorphicConstantToUPolymorphicConstant(x: PolymorphicConstant): UntypedTerm = UPolymorphicConstant(x)
  implicit def monomorphicConstantToUMonomorphicConstant(x: Term): UntypedTerm = UMonomorphicConstant(x)
  implicit def symbolToUVar(x: Symbol): UVar = UVar(x.name)

  implicit class UTOps[T <% UntypedTerm](body: T) {
    def ->:(arg: UVar): UntypedTerm = {
      UAbs(arg, body)
    }
    // Require at least one argument.
    def apply(that: UntypedTerm, more: UntypedTerm*): UApp = {
      more.foldLeft(UApp(body, that))((acc: UApp, arg: UntypedTerm) => UApp(acc, arg))
    }
  }
}
