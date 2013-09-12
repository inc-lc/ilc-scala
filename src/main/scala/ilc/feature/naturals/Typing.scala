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

    // FoldNat is polymorphic, hence it must be handled through TypedConst.
    // XXX However, this handling seems to be not done in typeOf. Is this code dead?
    case _ =>
      super.typeOf(c)
  }
}
