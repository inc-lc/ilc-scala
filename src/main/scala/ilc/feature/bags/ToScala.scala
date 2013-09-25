package ilc
package feature
package bags

trait ToScala extends base.ToScala with Syntax {
  private[this] def mapTypes(v: Type): (String, String) = {
    val vType = toScala(v)
    ("Map[%s, Int]".format(vType), vType)
  }

  override def toScala(t: Term): String =
    t match {
      case EmptyBag(v) =>
        val (mType, vType) = mapTypes(v)
        //s"Map.empty[$mType, Int]"
        s"bagEmpty[$vType]"
      case Singleton(v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagSingleton[$vType] _)"
      case Union(v) =>
        val (mType, vType) = mapTypes(v)
        //s"((b1: $mType) => (b2: $mType) => bagUnion(b1, b2))"
        s"(bagUnion[$vType] _)"
      case Negate(v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagNegate[$vType] _)"
      case FoldGroup(b, v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagFoldGroup[${toScala(b)}, $vType] _)"
      case _ =>
        super.toScala(t)
    }

  override def toScala(tau: Type): String =
    tau match {
      case BagType(valType) =>
        mapTypes(valType)._1

      case _ =>
        super.toScala(tau)
    }
}
