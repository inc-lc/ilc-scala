package ilc
package feature
package unit

trait Types extends base.Types {
  case object UnitType extends Type {
    override def toString = UnicodeOutput.choose("𝟙", "1")
  }
}
