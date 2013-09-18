package ilc
package feature
package naturals

trait TypedSyntax
extends base.TypedSyntax
   with Syntax
   with Typing
{
  // foldNat : r → (r → r) → Nat → r
  object TypedFoldNat {
    def apply(r: Type): Term =
      TypedConst(FoldNat, r =>: (r =>: r) =>: NatType =>: r)

    def unapply(t: Term): Option[Type] = t match {
      case TypedConst(FoldNat, r =>: _) =>
          Some(r)

      case _ =>
        None
    }
  }
}
