package ilc
package feature
package unit

trait Typing extends base.Typing {
  this: Syntax =>

  case object UnitType extends Type

  override def typeOf(c: Constant): Type = c match {
    case UnitTerm =>
      UnitType

    case _ =>
      super.typeOf(c)
  }
}
