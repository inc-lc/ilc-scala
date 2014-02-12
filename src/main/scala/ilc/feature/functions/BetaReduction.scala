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
            //XXX: if argNorm is a function, this produces new redexes!
          case _ =>
            App(funNorm, argNorm)
        }
      case _ =>
        //We need to assume this is a constant or a variable.
        t
    }
  }

  def betaNorm(t: Term) = {
    import Normalize._
    reify(eval(t, Map.empty))
  }

  object Normalize {
    sealed trait Value
    case class FunVal(fun: Value => Value, varName: Name, varType: Type, doInline: Boolean) extends Value
    case class TermVal(term: Term) extends Value
    case class AppVal(fun: Value, arg: Value) extends Value

    def eval(t: Term, env: Map[Name, Value]): Value =
      t match {
        case Abs(x, t) =>
          //XXX: compute doInline more cleverly.
          FunVal((arg: Value) => eval(t, env.updated(x.getName, arg)), x.getName, x.getType, true)
        case App(s, t) =>
          val arg = eval(t, env)
          eval(s, env) match {
            case FunVal(f, _, _, true) =>
              f(arg)
            case nonFunVal =>
              AppVal(nonFunVal, arg)
          }
        case variable: Variable =>
          env(variable.getName)
        case _ =>
          TermVal(t)
      }

    //Have a very simple and reliable fresh variable generator. Tracking free
    //variables might have been the performance killer of the other normalizer.
    var index = 0
    def fresh(varName: Name, varType: Type): Var = {
      index += 1
      Var(IndexedName("z", index), varType)
    }
    def fresh: Var => Var = {
      case Var(varName, varType) => fresh(varName, varType)
    }

    def reify(t: Value): Term =
      t match {
        case FunVal(f, varName, varType, _) => {
          val x = fresh(varName, varType)
          Abs(x, reify(f(TermVal(x))))
        }
        case TermVal(term) =>
          term
        case AppVal(fun, arg) =>
          App(reify(fun), reify(arg))
      }
  }
  def betaNormalize2(t: Term, env: Map[Name, Term]): Term =
    t match {
      case v: Variable =>
        env(v.getName)
      case App(fun, arg) =>
        //Capture is prevented by the freshening inside Abs.
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
        //Freshen the variable, to prevent capture in the case for application. Thanks @Toxaris for the hint.
        val w = fresh(v)
        Abs(w, betaNormalize2(body, env + (v.getName -> w)))
      case _ =>
        t
    }
}
