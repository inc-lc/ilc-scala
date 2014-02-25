package ilc
package feature
package integers

trait Types extends base.Types {
  case object IntType extends Type {
    override def toString = "ℤ"
    // This is needed for ilc.feature.inference.Reflection.reflectiveCopy
    def copy = IntType
  }
}
