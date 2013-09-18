package ilc
package feature
package naturals

// Reification is a subclass of Evaluation for value declarations
// alone. Should we separate those into a new trait, naturals.Values?
trait Reification extends base.Reification with Evaluation {
  override def reify(value: Value, valueType: Type): Term = value match {
    case NatValue(n) =>
      Nat(n)

    case _ =>
      super.reify(value, valueType)
  }
}
