package ilc
package feature
package let

import base.TypeError

trait ProgramSize extends Syntax {
  def termSize: Term => Int = {
    case App(f, t) => 1 + termSize(f) + termSize(t)
    case Abs(v, body) => 1 + termSize(v) + termSize(body)
    case Let(v, exp, body) => 1 + termSize(v) + termSize(exp) + termSize(body)
    case _ => 1
  }
}

trait BetaReduction extends Syntax with FreeVariables with analysis.Occurrences with Traversals with IsAtomic with metaprogs.AlphaEquivLet {
  outer =>
  private val freshGen = new base.FreshGen {
    /*
     * The singleton type annotation encodes a "sharing constraint"*:
     * freshGen.syntax.type =:= outer.type. The typechecker can deduce then
     * that freshGen.syntax.Term =:= outer.Term.
     *
     * * In the ML module system sense.
     */
    val syntax: outer.type = outer
  }
  implicitly[freshGen.syntax.type =:= outer.type] //A type equality assertion.
  implicitly[freshGen.syntax.Term =:= outer.Term]
  import freshGen.fresh

  val shouldNormalize = true

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
    //Reify can produce lets, so they can be passed to eval.
    reify(eval(t, Map.empty))
  }

  @annotation.tailrec final def doNormalize(t: Term): Term = {
    //Because of how usage counts are computed (before normalization), this is not idempotent.
    //That's also typical in shrinking reductions.
    //Let's run this to convergence.
    val u = normalizeEverywhereOnce(t)
    if (alphaEquiv(t, u))
      t
    else
      doNormalize(u)
  }

  def normalize(t: Term) =
    if (shouldNormalize)
      doNormalize(t)
    else
      t


  /**
   * A "custom" implementation of normalization by evaluation.
   * The initial code was written and tested based on a faint recollection of the basic algorithm,
   * but it worked surprisingly well.
   * Warning: this is completely syntax-directed, so it neither eta-expands not
   * eta-reduces terms; in other words, it does not compute a beta-eta-normal form,
   * only a beta-normal form.
   *
   * Unusual features:
   * - instead of producing standard normal forms, it performs let conversion and shrinking reductions.
   *   In other words, beta-redexes are converted to lets, lets which are used at most once are inlined.
   * - Constants are simply preserved in values and in the output.
   * - Therefore, instead of using an evaluator, we use a home-grown *partial* evaluator which residualizes
   *   lots of the original structure.
   */
  object Normalize {
    /**
     * Representation of residual values for our partial evaluator.
     */
    sealed trait Value

    /**
     * Functions are represented using HOAS; they also remember the name of the original variable (which is used, when possible,
     * instead of generating a fresh variable, or as a basis for variable renaming), and they cache a flag @param doInline
     * specifying whether this function uses its argument only once.
     */
    case class FunVal(fun: Value => Value, v: Var, doInline: Boolean) extends Value

    /**
     * A neutral term is a normal form which is not a lambda abstraction: that
     * is, usually, a variable, a constant or an application.
     * We use the same concept in our values.
     */
    sealed trait NeutralValue extends Value

    //Variables or constants.
    case class TermVal(term: Term) extends NeutralValue {
      require(isAtomic(term))
    }
    /**
     * Residualized application. It can only contain a neutral residual term in the function position.
     */
    case class AppVal(fun: NeutralValue, arg: Value) extends NeutralValue
    case class LetVal(v: Var, varDef: Value, body: Value => Value) extends NeutralValue

    //compute doInline by counting occurrences, turning this into shrinking reductions.
    //Note: t has not been normalized yet here, and when we do inlining we
    //don't get the actual value.
    //
    //occurrencesOf handles also Let nodes. We could desugar them before,
    //but occurrencesOf can easily be more precise if redexes are transformed into lets before, and this makes a difference in the output size.
    def precomputeDoInline(x: Var, t: Term) = (t occurrencesOf x) != UsageCount.more
    //Requirement: this can only return true for something which is safe to inline - in particular, its cost must be bounded.
    def doInlineHeuristics(fv: FunVal, arg: Value) = fv.doInline || isTrivial(arg)
    //Possible TODO: inline arg if it is a lambda and it is only/sometimes used in the function position of an application.
    //Right now, we unconditionally inline lambdas, which is not necessarily helpful.

    //This flag can be altered without affecting correctness, only performance/code size of the output.
    private val inlineLambdaAbstractions = true
    //TODO: Move it to analysis to allow for more trivial terms. In particular, to allow (x + 1) we'd need a "trivialConstant" predicate.
    def isTrivial(arg: Value): Boolean =
      arg match {
        case TermVal(_) => true
        //Duplicating a function value can increase code size, but will not
        //duplicate work, because evaluating a closure requires only packaging
        //the code pointer and the environment.

        //Since the environment only ever contains values which can be inlined
        //safely, we need not fear that duplicating the environment and then
        //reifying it will duplicate computation. (When applying a function to
        //an expensive computation, we create a Let node, and the environment
        //will only ever contain a residualized variable.

        case fv: FunVal => inlineLambdaAbstractions
        case _ => false
      }

    /**
     * Evaluate a term @param t to a value given an environment @param env.
     * This started out as a mostly standard evaluator — see the case for abstractions.
     * However, it became a partial evaluator.
     */
    //t can contain Let nodes.
    def eval(t: Term, env: Map[Name, Value]): Value =
      t match {
        case Abs(x, t) =>
          //This is the only creator of functions used in FunVal. We could defunctionalize it!
          //The argument to function is placed in the environment; this is used to implement closures.
          FunVal(arg => eval(t, env.updated(x.getName, arg)), x, precomputeDoInline(x, t))
        case App(s, t) =>
          val arg = eval(t, env)

          def findFun(fun: Value): Value =
            fun match {
              case fv @ FunVal(f, v, _) =>
                //If the argument does not get duplicated, or can be duplicated without penalty, we call f, which will place it into the environment and
                //allow inlining it.
                if (doInlineHeuristics(fv, arg))
                  f(arg)
                else
                  //Otherwise, we create a let binding which prevents duplicating the argument.
                  //Instead of placing arg into the environment, we residualize the environment into a let binding, preserving sharing.
                  //In other words, closures are implemented through let bindings.
                  LetVal(v, arg, f)

              case LetVal(v, arg, f) =>
                //Names are decorations here, because we are using HOAS: so we can reuse
                //v without risk.
                LetVal(v, arg, hoasArg => findFun(f(hoasArg)))

              case nonFunVal: NeutralValue =>
                AppVal(nonFunVal, arg)
            }

          findFun(eval(s, env))

        case Var(name, _) =>
          //This inlines values from the environment.
          env get name getOrElse (throw new TypeError(s"Variable $name not available in context $env, ill-scoped term"))

        case Let(v, varDef, body) =>
          //This would perform full inlining for such lets. So we don't do that.
          //eval(body, env.updated(v.getName, eval(varDef, env)))

          //We could duplicate the complete logic for the App case, but it's easier to just desugar lets.
          //We could do that before NbE, but that makes the work of precomputeDoInline harder.

          //Hence, instead, do *ONE* desugaring step just when needed.
          //Technically, this makes eval syntactically non-compositional, but if call the correct function here,
          //this will still be correct (also, this eval is already highly non-compositional).
          eval(desugarLetRule(t), env)
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
        //Note that this preserves the sharing created when residualizing let bindings.
        case LetVal(v, varDef, f) => {
          val x = fresh(v)
          Let(x, reify(varDef), reify(f(TermVal(x))))
        }
      }
  }
}
