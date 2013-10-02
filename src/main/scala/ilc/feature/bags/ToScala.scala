package ilc
package feature
package bags

trait ToScala
extends base.ToScala
   with Syntax
{
  private[this] def mapTypes(v: Type): (String, String) = {
    val vType = toScala(v)
    ("Bag[%s]".format(vType), vType)
  }

  override def toScala(t: Term): String =
    t match {
      case EmptyBag(v) =>
        val (mType, vType) = mapTypes(v)
        s"bagEmpty[$vType]"
      case Singleton(v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagSingleton[$vType] _)"
      case Union(v) =>
        val (mType, vType) = mapTypes(v)
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
