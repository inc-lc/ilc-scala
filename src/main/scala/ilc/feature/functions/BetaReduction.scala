package ilc
package feature
package functions

trait BetaReduction extends Syntax with analysis.FreeVariables {
  def subst(toReplace: Variable, replacement: Term): (TypingContext, Term) => Term = {
    def go(typingContext: TypingContext, replaceIn: Term): Term =
      replaceIn match {
        case v: Variable =>
          if (toReplace == v)
            replacement
          else
            replaceIn
        case Abs(v, body) =>
          if (toReplace == v) //toReplace is shadowed
            replaceIn
          else { // (toReplace != v)
            val freeVars = replacement.freeVariables
            val extendedTypingContext = v +: (typingContext ++ freeVars)
            val vPrime = Var(freshName(extendedTypingContext, v.getName), v.getType)
            ////XXX this might be bigger than needed.
            //val extContext = vPrime +: extendedTypingContext

            //This typing context needs to be bigger than usual, since we're
            //moving the term to a different lambda context.
            val extContext = vPrime +: v +: typingContext
            val alphaRenamedBody = subst(v, vPrime)(extContext, body)
            Abs(vPrime, go(extContext, alphaRenamedBody))
          }
        case App(fun, arg) =>
          App(go(typingContext, fun), go(typingContext, arg))
        case _ => replaceIn
      }
    go _
  }
  //Use base.Syntax#Term ?

  def betaNormSubst(t: Term) = betaNormalize(t, TypingContext.empty)

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
            subst(v, argNorm)(typingContext, body)
          case _ =>
            App(funNorm, argNorm)
        }
      case _ =>
        //We need to assume this is a constant or a variable.
        t
    }
  }

  def betaNorm(t: Term) = betaNormalize2(t, Map.empty)
  def betaNormalize2(t: Term, env: Map[Name, Term]): Term =
    t match {
      case v: Variable =>
        env(v.getName)
      case App(fun, arg) =>
        //This fails because normArg can contain capture error, which become
        //visible as soon as we normalize a term containing a normalized term!

        val normFun = betaNormalize2(fun, env)
        val normArg = betaNormalize2(arg, env)
        normFun match {
          case Abs(v, body) =>
            //Here capture is possible.
            betaNormalize2(body, env + (v.getName -> normArg))
          case _ =>
            App(normFun, normArg)
        }
      case Abs(v, body) =>
        //Implement shadowing.
        Abs(v, betaNormalize2(body, env + (v.getName -> v)))
      case _ =>
        t
    }
}
