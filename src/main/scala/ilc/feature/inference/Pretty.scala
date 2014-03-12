package ilc.feature.inference

import scala.language.implicitConversions

trait Pretty extends Inference {
  implicit def polymorphicConstantToUPolymorphicConstant(x: PolymorphicConstant): UntypedTerm = UPolymorphicConstant(x)
  implicit def monomorphicConstantToUMonomorphicConstant(x: Term): UntypedTerm = UMonomorphicConstant(x)
  implicit def symbolToUVar(x: Symbol): UVar = UVar(x.name)

  /*
   * The point of this implicit conversion is to trigger ambiguity errors in
   * implicit resolution, rather than failing silently and mysteriously. If you
   * see this conversion mentioned in an ambiguity error, see if you're
   * inheriting from Scalatest's Matchers or importing its members, and see if
   * you can easily stop doing that. If not, read on for the details.
   *
   * = Details =
   * This implicit conversion is usually unnecessary, because it is subsumed by
   * UTOps, even though applying UTOps is "harder" (it takes an extra implicit
   * parameter). However, ScalaTest "pimps" apply(Any) on Symbols for different
   * reasons inside org.scalatest.Matchers.
   * That implicit conversion is preferred to UTOps (which we don't want);
   * adding symbolToUTOps ensures that the ambiguity is at least detected.
   * (We could maybe ensure this version is preferred to Matchers by defining
   * this conversion inside a class inheriting from Matchers, but that seems too
   * much work and I'm not sure it'd actually work).
   */
  implicit def symbolToUTOps(x: Symbol) = UTOps(x)

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
