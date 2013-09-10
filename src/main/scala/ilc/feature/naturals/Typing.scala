package ilc
package feature
package naturals

trait Typing extends base.Typing {
  this: Syntax =>

  case object NatType extends Type

  override def typeOf(c: Constant): Type = c match {
    case Nat(_) =>
      NatType

    case Plus =>
      NatType =>: NatType

    // the type of FoldNat are left intentionally
    // unspecified. because constants are polymorphic, and
    // we don't have a polymorphic type system yet.
    //
    // the plan is to have a term constructor
    // TypedConstant(c: Constant, tau: Type)
    // that specifies the type of a constant at use site.

    case _ =>
      super.typeOf(c)
  }
}
