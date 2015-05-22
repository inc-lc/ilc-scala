package ilc
package feature
package booleans

trait ToScala
extends functions.ToScala
   with unit.ToScala
   with Syntax
{
  override def toScala(tau: Type): String = tau match {
    case BooleanType => "Boolean"
    case _           => super.toScala(tau)
  }

  override def isScalaPrimitive(tau: Type) = tau match {
    case BooleanType => true
    case _ => super.isScalaPrimitive(tau)
  }

  override def toUntypedScala(t: Term): String = t match {
    case True  => "true"
    case False => "false"

    // call-by-name
    case IfThenElse(resultType) => {
      val unit = toScala(UnitTerm)
      val body = s"if (condition) thenBranch($unit) else elseBranch($unit)"
      scalaFunction("condition", "thenBranch", "elseBranch")(body)
    }

    case _ =>
      super.toUntypedScala(t)
  }
}
