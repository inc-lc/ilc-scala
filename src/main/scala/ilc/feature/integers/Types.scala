package ilc
package feature
package integers

trait Types extends base.Types {
  case object IntType extends Type {
    override def toString = UnicodeOutput.choose("ℤ", "Z")
  }

  val ℤ = IntType
}
