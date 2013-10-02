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

  override def toScala(t: Term): String = t match {
    case True  => "true"
    case False => "false"

    // call-by-name
    //
    // Remark. Type ascription is a uniform way to
    // specify the type of primitive operators. Consider a helper
    // for type ascription in base.ToScala. It shall also squash
    // all worries about the correct way to specify the parameter
    // type of a function (#168).
    case IfThenElse(resultType) => {
      val unit = toScala(UnitTerm)
      val body = s"if (condition) thenBranch($unit) else elseBranch($unit)"
      val function = s"(condition => thenBranch => elseBranch => $body)"
      s"($function: ${toScala(t.getType)})"
    }
  }
}
