package ilc
package feature
package unit

trait Syntax extends base.Syntax with Types {
  // the inhabitant of the unit type
  case object UnitTerm extends Term {
    override def getType: Type = UnitType
  }
}
