package ilc
package feature
package maybe

trait ToScala extends base.ToScala with Syntax {
  override def toUntypedScala(t: Term): String = t match {
    case Maybe(a, b) =>
      scalaFunction("z", "f", "m")(s"m.fold[${toScala(b)}](z)(f)")

    case Nope(contentType) =>
      "None"

    case Just(contentType) =>
      "(Some.apply _)"

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case MaybeType(contentType) =>
      "Option[%s]".format(toScala(contentType))

    case _ =>
      super.toScala(tau)
  }
}
