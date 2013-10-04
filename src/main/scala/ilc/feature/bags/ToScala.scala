package ilc
package feature
package bags

trait ToScala
extends abelianGroups.ToScala
   with Syntax
{
  addLibrary("bags")

  private[this] def mapTypes(v: Type): (String, String) = {
    val vType = toScala(v)
    ("Bag[%s]".format(vType), vType)
  }

  override def toUntypedScala(t: Term): String =
    t match {
      case EmptyBag(v) =>
        s"bagEmpty"
      case Singleton(v) =>
        s"bagSingleton"
      case Union(v) =>
        s"bagUnion"
      case Negate(v) =>
        s"bagNegate"
      case FoldGroup(b, v) =>
        s"bagFoldGroup"
      case FreeAbelianGroup(v) =>
        s"FreeAbelianGroup()"
      case _ =>
        super.toUntypedScala(t)
    }

  override def toScala(tau: Type): String =
    tau match {
      case BagType(valType) =>
        mapTypes(valType)._1

      case _ =>
        super.toScala(tau)
    }
}
