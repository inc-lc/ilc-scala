package ilc
package feature
package abelianGroups

trait ToScala extends base.ToScala with Syntax {
  addLibrary("abelianGroups")

  override def toScala(tau: Type): String = tau match {
    case AbelianGroupType(e) =>
      s"AbelianGroup[${toScala(e)}]"

    case _ =>
      super.toScala(tau)
  }

  override def toUntypedScala(t: Term): String = {
    // get abelian group type
    // (only works if the first argument of t is
    // an abelian group)
    t match {
      case AbelianGroup(e) =>
        s"GenerativeGroup.curried[${toScala(e)}]"

      case GetBinOp(_)   => s"(_.binOp)"
      case GetInv(_)     => s"(_.inv)"
      case GetNeutral(_) => s"(_.neutral)"

      case AreEqualGroups(_) =>
        scalaFunction("ag1", "ag2")("ag1.isEqualGroup(ag2)")

      case _ =>
        super.toUntypedScala(t)
    }
  }
}
