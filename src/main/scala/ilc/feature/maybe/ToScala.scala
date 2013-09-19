package ilc
package feature
package maybe

trait ToScala extends base.ToScala with Syntax {
  override def toScala(t: Term): String = t match {
    case Maybe(aType, bType) => {
      val List(a, b) = List(aType, bType) map toScala
      val maybe_a = toScala(MaybeType(aType))
      val body = s"m.fold[$b](z)(f)"
      s"((z: $b) => (f: $a => $b) => (m: $maybe_a) => $body)"
    }

    case Nope(contentType) => {
      val maybeType = toScala(t.getType)
      s"(None: $maybeType)"
    }

    case Just(contentType) => {
      val tau = toScala(contentType)
      s"((x: $tau) => Some(x))"
    }

    case _ =>
      super.toScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case MaybeType(contentType) =>
      "Option[%s]".format(toScala(contentType))

    case _ =>
      super.toScala(tau)
  }
}
