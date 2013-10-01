package ilc
package feature
package booleans

trait Types extends base.Types with sums.Types with unit.Types {
  case object BooleanType extends Type
}
