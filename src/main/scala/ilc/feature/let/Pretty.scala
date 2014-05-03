package ilc
package feature
package let

trait Pretty extends functions.Pretty with Syntax {
  override def pretty(t: Term, priority: Priority) = t match {
    case Let(variable, exp, body) =>
      template(priorityOfAbs, priority,
               s"${variable.getName.toString} =${deeper(s"$indent${pretty(exp, outermostPriority)}")};${indent}${pretty(body, outermostPriority)}")
    case _ => super.pretty(t, priority)
  }
}
