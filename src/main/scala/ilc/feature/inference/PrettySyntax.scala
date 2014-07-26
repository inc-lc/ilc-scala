package ilc.feature.inference

import scala.language.implicitConversions

trait PrettySyntax extends Inference {
  implicit def untypedTermToTerm(t: UntypedTerm) =
    typedTermToTerm(inferType(t))
  implicit def polymorphicConstantToUPolymorphicConstant(x: PolymorphicConstant): UntypedTerm = UPolymorphicConstant(x)
  implicit def monomorphicConstantToUMonomorphicConstant(x: Term): UntypedTerm = UMonomorphicConstant(x)
  implicit def symbolToUVar(x: Symbol): UVar = UVar(x.name)
  implicit def stringToUVar(x: String): UVar = UVar(x)

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
  implicit def stringToUTOps(x: String) = UTOps(x)

  case class TypeAnnotation(name: String, typ: Type)

  implicit class UTOps[T <% UntypedTerm](untypedTerm: T) {

    //->: is right-associative, so untypedTerm is the right-hand side argument.
    def ->:(param: Symbol): UntypedTerm =
      UAbs(param.name, None, untypedTerm)

    def ->:(param: TypeAnnotation): UntypedTerm =
      UAbs(param.name, Some(param.typ), untypedTerm)

    def ->:(param: String): UntypedTerm =
      UAbs(param, None, untypedTerm)

    // Require at least one argument.
    def apply(that: UntypedTerm, more: UntypedTerm*): UApp =
      more.foldLeft(UApp(untypedTerm, that))((acc: UApp, arg: UntypedTerm) => UApp(acc, arg))

    def ofType(typ: Type): TypeAscription = TypeAscription(untypedTerm, typ)

    def composeWith(second: UntypedTerm): UntypedTerm =
      'x ->: untypedTerm(second('x))
}

  implicit class SymbolOps(name: Symbol) {
    def %(typ: Type): TypeAnnotation = TypeAnnotation(name.name, typ)
  }
}
