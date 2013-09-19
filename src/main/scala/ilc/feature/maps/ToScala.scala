package ilc
package feature
package maps

trait ToScala extends base.ToScala with Syntax {
  private[this] def mapTypes(k: Type, v: Type): (String, String, String) = {
    val (kType, vType) = (toScala(k), toScala(v))
    ("Map[%s, %s]".format(kType, vType), kType, vType)
  }

  override def toScala(t: Term): String = t match {
    case EmptyMap(k, v) => {
      val (_, kType, vType) = mapTypes(k, v)
      "Map.empty[%s, %s]".format(kType, vType)
    }

    case Update(k, v) => {
      val (mType, kType, vType) = mapTypes(k, v)
      s"((k: $kType) => (v: $vType) => (m: $mType) => m.updated(k, v))"
    }

    case Lookup(k, v) => {
      val (mType, kType, vType) = mapTypes(k, v)
      val maybe = toScala(MaybeType(v))
      val body = s"m.get(k)"
      s"((k: $kType) => (m: $mType) => $body)"
    }

    case Delete(k, v) => {
      val (mType, kType, vType) = mapTypes(k, v)
      s"((k: $kType) => (m: $mType) => m - k)"
    }

    case Fold(k, a, b) => {
      val (mType, kType, aType) = mapTypes(k, a)
      val bType = toScala(b)
      val fType = s"$kType => $aType => $bType => $bType"
      val body = s"m.foldRight[$bType](z)((p, b) => f(p._1)(p._2)(b))"
      s"((f: $fType) => (z: $bType) => (m: $mType) => $body)"
    }

    case _ =>
      super.toScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case MapType(keyType, valType) =>
      mapTypes(keyType, valType)._1

    case _ =>
      super.toScala(tau)
  }
}
