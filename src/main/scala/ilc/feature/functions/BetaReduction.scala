package ilc
package feature
package functions

trait BetaReduction extends Syntax {
  def subst(v: Variable, arg: Term, typingContext: TypingContext): Term => Term = {
    def go(body: Term): Term =
      body match {
        case v2: Variable =>
          if (v == v2)
            arg
          else
            body
        case Abs(v2, body2) =>
          if (v == v2) //v is shadowed
            body
          else { // (v != v2)
            val v3 = Var(freshName(typingContext, v2.getName), v2.getType)
            val extContext = v2 +: typingContext
            val notCapturedArg = subst(v2, v3, extContext)(arg)
            Abs(v2, subst(v, notCapturedArg, extContext)(body2))
          }
        case App(fun, arg2) =>
          App(go(fun), go(arg2))
        case _ => body
      }
    go _
  }
  //Use base.Syntax#Term ?

  def betaNorm(t: Term) = betaNormalize(t, TypingContext.empty)

  //Should this prevent code/work duplication?
  def betaNormalize(t: Term, typingContext: TypingContext): Term = {
    t match {
      case Abs(v, body) =>
        Abs(v, betaNormalize(body, v +: typingContext))
      case App(fun, arg) =>
        val funNorm = betaNormalize(fun, typingContext)
        //Normalizing the argument is not necessary. As long as the object
        //language is strongly normalizing, this step makes no difference.
        //If the object language is not strongly normalizing
        val argNorm = betaNormalize(arg, typingContext)
        funNorm match {
          case Abs(v, body) =>
            subst(v, argNorm, typingContext)(body)
          case _ =>
            App(funNorm, argNorm)
        }
      case _ =>
        //We need to assume this is a constant or a variable.
        t
    }
  }
}
