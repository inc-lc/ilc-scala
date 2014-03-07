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
    // Maybe we want a different way to give the type of arguments. Or we just allow any UntypedTerm.
    // I'm not so happy with this being in Pretty. Ascribing the parameter is not possible with plain UntypedTerms
    // because UAbs only takes an UVar as its argument, not a TypeAscription or UTypedTerm.
    def ->:(arg: TypeAscription): UntypedTerm = {
      TypeAscription(UAbs(arg.term.asInstanceOf[UVar], body), arg.typ =>: freshTypeVariable())
    }
    // Require at least one argument.
    def apply(that: UntypedTerm, more: UntypedTerm*): UApp = {
      more.foldLeft(UApp(body, that))((acc: UApp, arg: UntypedTerm) => UApp(acc, arg))
    }
    def ofType(typ: Type): TypeAscription = TypeAscription(body, typ)
  }
}
