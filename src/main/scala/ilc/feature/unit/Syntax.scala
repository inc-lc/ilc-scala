package ilc
package feature
package unit

trait Syntax extends base.Syntax {
  // the inhabitant of the unit type
  // also used as placeholder
  case object UnitTerm extends Constant
}
