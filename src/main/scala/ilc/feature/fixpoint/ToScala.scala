package ilc
package feature
package fixpoint

trait ToScala extends base.ToScala with Syntax {
  addLibrary("fixpoint")

  override def toUntypedScala(t: Term): String =
    t match {
      case Fix(t) =>
        "fix"
      case _ =>
        super.toUntypedScala(t)
    }
}
