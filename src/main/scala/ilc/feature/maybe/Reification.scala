package ilc
package feature
package maybe

trait Reification extends base.Reification with Evaluation {
  override def reify(value: Value, valueType: Type): Term =
    (value, valueType) match {
      case (MaybeValue(None), MaybeType(contentType)) =>
        Nope.tapply(contentType)

      case (MaybeValue(Some(content)), MaybeType(contentType)) =>
        Just ! reify(content, contentType)

      case _ =>
        super.reify(value, valueType)
    }
}
