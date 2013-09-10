package ilc
package feature
package sums

trait TypedSyntax
extends base.TypedSyntax
   with Syntax
   with Typing
{
  // either: (a → c) → (b → c) → a ⊎ b → c
  object TypedEither {
    def apply(a: Type, b: Type, c: Type): Term =
      TypedConst(Either, (a =>: c) =>: (b =>: c) =>: SumType(a, b) =>: c)

    def unapply(t: Term): Option[(Type, Type, Type)] = t match {
      case TypedConst(Either, (a =>: c) =>: (b =>: _) =>: _) =>
        Some((a, b, c))

      case _ =>
        None
    }
  }

  object TypedLeft {
    def apply(a: Type, b: Type): Term =
      TypedConst(Left, a =>: SumType(a, b))

    def unapply(t: Term): Option[(Type, Type)] = t match {
      case TypedConst(Left, _ =>: SumType(a, b)) =>
        Some((a, b))

      case _ =>
        None
    }
  }

  object TypedRight {
    def apply(a: Type, b: Type): Term =
      TypedConst(Right, b =>: SumType(a, b))

    def unapply(t: Term): Option[(Type, Type)] = t match {
      case TypedConst(Right, _ =>: SumType(a, b)) =>
        Some((a, b))

      case _ =>
        None
    }
  }
}
