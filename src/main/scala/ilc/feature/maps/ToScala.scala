package ilc
package feature
package maps

trait ToScala extends base.ToScala with Syntax {
  private[this] def mapTypes(k: Type, v: Type): (String, String, String) = {
    val (kType, vType) = (toScala(k), toScala(v))
    ("Map[%s, %s]".format(kType, vType), kType, vType)
  }

  override def toUntypedScala(t: Term): String = t match {
    case EmptyMap(k, v) =>
      "Map.empty"

    case Update(k, v) =>
      scalaFunction("k", "v", "m")("m.updated(k, v)")

    case Lookup(k, v) =>
      scalaFunction("k", "m")("m.get(k)")

    case Delete(k, v) =>
      scalaFunction("k", "m")("m - k")

    case Fold(k, a, b) =>
      scalaFunction("f", "z", "m") {
        s"m.foldRight[${toScala(b)}](z)((p, b) => f(p._1)(p._2)(b))"
      }

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case MapType(keyType, valType) =>
      mapTypes(keyType, valType)._1

    case _ =>
      super.toScala(tau)
  }
}
