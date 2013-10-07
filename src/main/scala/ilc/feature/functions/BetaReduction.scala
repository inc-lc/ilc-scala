package ilc
package feature
package functions

trait BetaReduction extends Syntax with analysis.FreeVariables {
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
            val freeVars = arg.freeVariables
            val extendedTypingContext = v2 +: TypingContext(typingContext.toList ++ freeVars)
            val v3 = Var(freshName(extendedTypingContext, v2.getName), v2.getType)
            ////XXX this might be bigger than needed.
            //val extContext = v3 +: extendedTypingContext

            //This typing context needs to be bigger than usual, since we're
            //moving the term to a different lambda context.
            val extContext = v3 +: v2 +: typingContext
            val alphaRenamedBody2 = subst(v2, v3, extContext)(body2)
            Abs(v3, subst(v, arg, extContext)(alphaRenamedBody2))
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
