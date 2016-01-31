package ilc
package feature
package maybe

trait Syntax
extends base.Syntax
   with Types
   with functions.Syntax // for application
{
  /** Mimics Haskell's Data.Maybe.mayte
    * {{{
    * maybe :: b -> (a -> b) -> Maybe a -> b
    * }}}
    */
  object Maybe extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("a", "b") {case Seq(a, b) =>
      b =>: (a =>: b) =>: MaybeType(a) =>: b
    }
  }

  object Nope extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("contentType") {
      contentType =>
        MaybeType(contentType)
    }
  }

  object Just extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("contentType") {
      contentType =>
        contentType =>: MaybeType(contentType)
    }
  }

  /** Maybe as a monad */
  implicit class maybeMonad[T <% TermBuilder](t: T) {
    def >>= (f: TermBuilder): TermBuilder =
      context => {
        val tBuilder: TermBuilder = t
        val tTerm: Term = t(context).toTerm
        val inputType = tTerm.getType match {
          case MaybeType(contentType) =>
            contentType

          case wrongType =>
            typeErrorNotTheSame("binding a maybe monad",
              "Maybe a", wrongType)
        }
        val fTerm: Term = f(context).specialize(inputType)
        val outputType = fTerm.getType match {
          case a =>: MaybeType(b) if a == inputType =>
            b

          case wrongType =>
            typeErrorNotTheSame("binding a maybe monad",
              s"$inputType =>: MaybeType(b)", wrongType)
        }
        (Maybe ! Nope.tapply(outputType) ! fTerm ! tTerm)(context)
      }
  }
}
