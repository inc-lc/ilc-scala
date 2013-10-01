package ilc
package feature
package booleans

trait ToScala extends base.ToScala with Syntax {
  override def toScala(tau: Type): String = tau match {
    case BooleanType => "Boolean"
    case _           => super.toScala(tau)
  }

  override def toScala(t: Term): String = t match {
    case True  => "true"
    case False => "false"

    // call-by-name
    case IfThenElse(resultType) => {
      val r = toScala(resultType)
      "((condition: Boolean) => " +
        "(thenBranch: => $r) => " +
        "(elseBranch: => $r) => " +
          "if (condition) thenBranch else elseBranch)"
    }
  }
}
