package ilc
package feature
package base

trait Reification extends Evaluation {
  def reify(value: Value, valueType: Type): Term =
    throw IDontKnow(s"how to reify $value of type $valueType")
}
