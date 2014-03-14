package ilc
package feature
package functions

trait LetSyntax extends Syntax {
  //Argh! Most traversals are not ready for Let!
  case class Let(variable: Var, exp: Term, body: Term) extends Term {
    override lazy val getType = {
      assert (variable.getType == exp.getType)
      body.getType
    }
  }
}

trait LetToScala extends LetSyntax with ToScala {
  override def toUntypedScala(t: Term) = {
    t match {
      case Let(v, exp, body) =>
        //This does not work when v shadows an existing variable that is used in exp.
        //This shows up in one (1!) case in Histogram - that's it.
        //However, normalization-by-evaluation freshens the variable, preventing this bug.
        //XXX: document the exact interfaces, since we have an interaction between slightly different interfaces (nominal vs. something which I hope is Barendregt), in the area of binding.
        s"""${openBrace()}
        |${indentNoNl()}lazy val ${toUntypedScala(v)} = ${toScala(exp)}
        |${indentNoNl()}${toScala(body)}
        |${closeBrace()}""".stripMargin
      case _ => super.toUntypedScala(t)
    }
  }
}

trait FreeVariablesForLet extends analysis.FreeVariables with LetSyntax {
  override def termFreeVariables(term: Term): Set[Var] = term match {
    case Let(v, exp, body) =>
      //If v is free in exp, it is indeed free in the overall let!
      body.freeVariables - v ++ exp.freeVariables
    case _ =>
      super.termFreeVariables(term)
  }
}

trait Traversals extends LetSyntax {
  type =?>:[A, B] = PartialFunction[A, B]

  def orIdentity[T](f: T =?>: T): T => T =
    x => f applyOrElse (x, identity[T])

  //Switch to shapeless?
  //Probably yes, since I had to debug this, and it needs to be extended for Let (as lots of existing code), and so on.
  def everywhere: (Term => Term) => (Term => Term) =
    transf => term =>
      transf(term match {
        case App(f, t) => App(everywhere(transf)(f), everywhere(transf)(t))
        case Abs(v, body) => Abs(v, everywhere(transf)(body))
        case Let(v, exp, body) => Let(v, everywhere(transf)(exp), everywhere(transf)(body))
        case other =>
          other
      })
}

trait ProgramSize extends LetSyntax {
  def termSize: Term => Int = {
    case App(f, t) => 1 + termSize(f) + termSize(t)
    case Abs(v, body) => 1 + termSize(v) + termSize(body)
    case Let(v, exp, body) => 1 + termSize(v) + termSize(exp) + termSize(body)
    case _ => 1
  }
}

trait BetaReduction extends Syntax with LetSyntax with FreeVariablesForLet with analysis.Occurrences with Traversals with LetToScala {
  val shouldNormalize = true

  def subst(toReplace: Var, replacement: Term): (TypingContext, Term) => Term = {
    def go(typingContext: TypingContext, replaceIn: Term): Term =
      replaceIn match {
        case v: Var =>
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

  def letBetaReduceRule: Term =?>: Term = {
    case App(Abs(v, body), arg) => Let(v, arg, body)
  }

  def dceRule: Term =?>: Term = {
    case Let(v, arg, body) if !(body.freeVariables contains v) => body
  }

  def desugarLetRule: Term =?>: Term = {
    case Let(v, arg, body) => App(Abs(v, body), arg)
  }

  val letBetaReduceRuleTotal = orIdentity(letBetaReduceRule)

  val letBetaReduceOneStep = everywhere(letBetaReduceRuleTotal)

  val dceOneStep = everywhere(orIdentity(dceRule))

  val desugarLet = everywhere(orIdentity(desugarLetRule))

  def normalizeEverywhereOnce(t: Term) = {
    import Normalize._
    //Reify can produce lets, so there's a point in desugaring them before calling eval.
    reify(eval(desugarLet(t), Map.empty))
//    desugarLet(dceOneStep(letBetaReduceOneStep(t)))
  }

  def doNormalize(t: Term): Term = {
    //Because of how usage counts are computed (before normalization), this is not idempotent.
    //That's also typical in shrinking reductions.
    //Let's run this to convergence.
    //Since I'm too lazy to implement alpha-equivalence testing, especially
    //in an efficient way, just reset the freshness index.
    resetIndex()
    val u = normalizeEverywhereOnce(t)
    if (t == u)
      t
    else
      normalize(u)
  }

  def normalize(t: Term) =
    if (shouldNormalize)
      doNormalize(t)
    else
      t


  object Normalize {
    sealed trait Value
    case class FunVal(fun: Value => Value, v: Var, doInline: Boolean) extends Value
    case class TermVal(term: Term) extends Value
    case class AppVal(fun: Value, arg: Value) extends Value
    case class LetVal(v: Var, varDef: Value, body: Value => Value) extends Value

    //compute doInline by counting occurrences, turning this into shrinking reductions.
    //Note: t has not been normalized yet here, and when we do inlining we
    //don't get the actual value.
    //XXX: use letBetaReduceOneStep(t) instead of t?
    //XXX: must occurrencesOf handle also Let nodes? Strictly speaking no, because the input to eval cannot contain Let nodes.
    def precomputeDoInline(x: Var, t: Term) = (t occurrencesOf x) != UsageCount.more
    def doInlineHeuristics(fv: FunVal, arg: Value) = fv.doInline || isTrivial(arg)

    //Move it to analysis to allow for more trivial terms.
    def isTrivial(arg: Value): Boolean =
      arg match {
        case TermVal(Abs(_, _)) => false
        case TermVal(App(_, _)) => false
        //Variables or constants.
        case TermVal(_) => true
        case _ => false
      }

    //t cannot contain Let nodes.
    def eval(t: Term, env: Map[Name, Value]): Value =
      t match {
        case Abs(x, t) =>
          FunVal(arg => eval(t, env.updated(x.getName, arg)), x, precomputeDoInline(x, t))
        case App(s, t) =>
          val arg = eval(t, env)
          def findFun(fun: Value): Value =
            fun match {
              case fv @ FunVal(f, v, _) =>
                if (doInlineHeuristics(fv, arg))
                  f(arg)
                else
                  LetVal(v, arg, f)
              case LetVal(v, arg, f) =>
                //Names are decorations here, we are using HOAS: so we can reuse
                //v without risk.
                LetVal(v, arg, hoasArg => findFun(f(hoasArg)))
              case nonFunVal =>
                AppVal(nonFunVal, arg)
            }

          val fun = eval(s, env)
          findFun(fun)
        case Var(name, _) =>
          env(name)
        case Let(v, varDef, body) =>
          //This would perform full inlining for such lets. We could duplicate the complete logic for the App case, but it's easier to desugar lets before NbE.
          //eval(body, env.updated(v.getName, eval(varDef, env)))
          throw new RuntimeException("Unexpected let node '${t}' inside Normalize.eval")
        case _ =>
          TermVal(t)
      }

    def reify(t: Value): Term =
      t match {
        case FunVal(f, v, _) => {
          val x = fresh(v)
          Abs(x, reify(f(TermVal(x))))
        }
        case TermVal(term) =>
          term
        case AppVal(fun, arg) =>
          App(reify(fun), reify(arg))
        //The behavior here can be deduced from the behavior for AppVal(FunVal(f, v, _), varDef)
        case LetVal(v, varDef, f) => {
          val x = fresh(v)
          Let(x, reify(varDef), reify(f(TermVal(x))))
        }
      }
  }

  //Have a very simple and reliable fresh variable generator. Tracking free
  //variables might have been the performance killer of the other normalizer.
  var index = 0
  def resetIndex() {
    index = 0
  }
  def fresh(varName: Name, varType: Type): Var = {
    index += 1
    Var(IndexedName("z", index), varType)
    //For extra readability, during debugging. This increases the output size (by around 2%).
    //Var(IndexedName(varName, index), varType)
  }

  def fresh(v: Var): Var = fresh(v.getName, v.getType)

  def betaNormalize2(t: Term, env: Map[Name, Term]): Term =
    t match {
      case Var(name, _) =>
        env(name)
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
