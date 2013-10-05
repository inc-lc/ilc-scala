package ilc
package feature
package abelianMaps

trait ToScala
extends maps.ToScala
   with abelianGroups.ToScala
   with Syntax
{
  addLibrary("abelianMaps")

  override def toScala(tau: Type): String = tau match {
    case MapType(keyType, valType) =>
      s"AbelianMap[${toScala(keyType)}, ${toScala(valType)}]"

    case _ =>
      super.toScala(tau)
  }

  override def toUntypedScala(t: Term): String = t match {
    case EmptyMap(k, v)     => "emptyMap"
    case SingletonMap(k, v) => "singletonMap"
    case LiftGroup(k, v)    => "liftGroup"
    case FoldByHom(k, a, b) => "foldByHom"

    case _ =>
      super.toUntypedScala(t)
  }
}
