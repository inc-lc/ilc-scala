package ilc
package feature
package abelianGroups

trait ToScala extends base.ToScala with Syntax {
  override def toScala(tau: Type): String = tau match {
    case AbelianGroupType(e) =>
      s"AbelianGroup[${toScala(e)}]"

    case _ =>
      super.toScala(tau)
  }

  override def toScala(t: Term): String = {
    // get abelian group type
    // (only works if the first argument of t is
    // an abelian group)
    def agType: String = toScala(getArgumentTypes(t.getType).head)
    t match {
      case AbelianGroup(e) =>
        s"GenerativeGroup.curried[${toScala(e)}]"

      case GetBinOp(_)   => s"((ag: $agType) => ag.binOp)"
      case GetInv(_)     => s"((ag: $agType) => ag.inv)"
      case GetNeutral(_) => s"((ag: $agType) => ag.neutral)"

      case AreEqualGroups(_) =>
        s"((ag1: $agType) => (ag2: $agType) => ag1.isEqualGroup(ag2))"

      case _ =>
        super.toScala(t)
    }
  }
}
