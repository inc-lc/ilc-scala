package ilc
package feature
package let

import scalaz._
import scala.collection.generic.Growable

trait ANormalFormInterface {
  this: Syntax =>
  def aNormalizeTerm(t: Term): Term
}
/**
 * Implementation of A-normalization, based on http://matt.might.net/articles/a-normalization/.
 * Written in CPS, so stack usage might be a problem. Should that ever happen, trampolining is an alternative.
 * Alternatively, one could implement this using a writer monad, as suggested by Tillmann.
 */
trait ANormalForm extends Syntax with IsAtomic with ANormalFormInterface {
  outer =>
  private val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  override def aNormalizeTerm(t: Term): Term = aNormalize(t)(identity)
  def aNormalize(t: Term)(k: Term => Term): Term = t match {
    case Abs(v, body) =>
      k(Abs(v, aNormalizeTerm(body)))
    case App(operator, operand) =>
      aNormalizeName(operator) { s =>
        aNormalizeName(operand) { t =>
          k(App(s, t))
        }
      }
    case Let(variable, exp, body) =>
      aNormalize(exp) { normalExp =>
        Let(variable, normalExp, aNormalize(body)(k))
      }
    case _ if isAtomic(t) => k(t)
  }

  def aNormalizeName(t: Term)(k: Term => Term) = {
    aNormalize(t) { normalT =>
      if (isAtomic(normalT))
        k(normalT)
      else {
        val newV = fresh("a", normalT.getType)
        Let(newV, normalT, k(newV))
      }
    }
  }
}

//
trait ANormalFormStateful extends Syntax with IsAtomic with ANormalFormInterface {
  outer =>
  private val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  import collection.mutable

  abstract class Bindings(val substs: mutable.Map[Var, Term] = mutable.Map.empty) {
    val bindings: mutable.Traversable[(Term, Var)] with Growable[(Term, Var)]
    def += (p: (Term, Var)): Unit = {
      bindings += p
    }
    def lookup(t: Term): Option[Var]
    def isCSE: Boolean
  }
  class CSEBindings extends Bindings {
    //Stores all bindings in order & prevent duplicates
    override val bindings = mutable.LinkedHashMap.empty[Term, Var]
    //Reuse bindings if needed.
    def lookup(t: Term): Option[Var] = bindings get t
    def isCSE = true
  }
  class NonCSEBindings extends Bindings {
    //Stores all bindings in order & keep duplicates.
    override val bindings = mutable.ListBuffer.empty[(Term, Var)]
    //Never reuse an existing binding.
    def lookup(t: Term): Option[Var] = None
    def isCSE = false
  }
  val doCSE = true
  val copyPropagation = true
  val partialApplicationsAreSpecial = true

  def createBindings() = if (doCSE) new CSEBindings else new NonCSEBindings

  override def aNormalizeTerm(t: Term): Term = {
    val bindings = createBindings()
    val normalT = aNormalize(t, bindings)
    bindings.bindings.foldRight(normalT) {
      case ((term, variable), t) =>
        Let(variable, term, t)
    }
  }

  /*
   * The only mutable state we use are the (global) fresh variable generator,
   * and the mutable maps threaded through as parameters, initialized by calls
   * to aNormalizeTerm (at the top-level and inside each lambda).
   */
  def aNormalize(t: Term, bindings: Bindings): Term = t match {
    case Abs(v, body) =>
      Abs(v, aNormalizeTerm(body))
    case App(operator, operand) =>
      def collectApps(t: Term, acc: List[Term]): List[Term] = t match {
        case App(s, t) => collectApps(s, t :: acc)
        case _ => t :: acc
      }
      //(Conditional) special handling for nested applications: Don't save an intermediate result for each intermediate node.
      //Note that this is very syntactic, and that's good: if the user already inserted a binding for a partial application, as in:
      // val r1 = f arg1 arg2
      // val r2 = r1 arg3
      //we don't want to inline r1 into r2, because that might lead to work duplication.
      //Here, we assume that the user inserted all needed sharing already.
      val operands =
        if (partialApplicationsAreSpecial)
          collectApps(operator, operand :: Nil)
        else
          operator :: operand :: Nil
      //Can't call aNormalizeName since it first calls aNormalize.
      aNormalizeName2(operands map (aNormalizeName(_)(bindings)) reduceLeft (App))(bindings)
    case Let(variable, exp, body) =>
      val normalExp = aNormalizeName(exp, Some(variable))(bindings)
      aNormalize(body, bindings)
    case v: Var =>
      (bindings.substs get v) getOrElse v
    case _ if isAtomic(t) => t
  }

  def aNormalizeName(t: Term, boundVarOpt: Option[Var] = None)(bindings: Bindings): Term = {
    aNormalizeName2(aNormalize(t, bindings), boundVarOpt)(bindings)
  }

  def aNormalizeName2(normalT: Term, boundVarOpt: Option[Var] = None)(bindings: Bindings): Term = {
    def bind(): Var = {
      val newV = boundVarOpt getOrElse fresh("a", normalT.getType)
      bindings += normalT -> newV
      newV
    }
    def reuse(existingTerm: Term): Term = {
      boundVarOpt foreach { boundVar =>
        //We can't just drop an existing binding, we need to record a substitution.

        //This implements copy propagation.
        assert(isAtomic(normalT) && copyPropagation || doCSE) //Validate non-local reasoning: we only get here
        //if copyPropagation is enabled, because of the if below, or if doCSE is enabled
        bindings.substs += boundVar -> existingTerm
      }
      existingTerm
    }
    if (isAtomic(normalT) && (copyPropagation || boundVarOpt.isEmpty)) {
      reuse(normalT)
    } else {
      import std.option.optionSyntax._
      //Prevents calling bind() for a new binding of the same body.
      bindings lookup normalT some (reuse) none (bind())
    }
  }
}

/**
  * Implement, in essence, half of phase 1 of "Caching intermediate results for program improvement",
  * (Liu and Teitelbaum, PEPM 1995) for our lambda-calculus extended with at least
  * Let and pairs. Basically, this simply means returning all intermediate results.
  *
  * The other half is adapting every function call to cope with this API change: the caller needs to extract the first component of the return value.
  * That's much more confused in my head and in the code.
  * One reason is that they deal only with the first-order case.
  * Other interesting differences between what I try to do and their alg. (in Fig. 3):
  * - we encode tuples as right-nested pairs, so their rst becomes snd.
  */
trait AddCaches extends ANormalFormStateful with products.SyntaxSugar with Traversals {
  outer =>
  private val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  //This is not supported yet for this analysis — I think this would require a concept of arity to be type-safe.
  override val partialApplicationsAreSpecial = false

  //From aNormalize, calling addCaches instead of aNormalizeTerm
  //XXX aNormalizeName still calls aNormalize instead of aNormalizeAddCaches. To fix this, stop the copy-pasting and the mixing-in.
  def aNormalizeAddCaches(t: Term, bindings: Bindings): Term = t match {
    case Abs(v, body) =>
      Abs(v, addCaches(body))
    case App(operator, operand) =>
      def collectApps(t: Term, acc: List[Term]): List[Term] = t match {
        case App(s, t) => collectApps(s, t :: acc)
        case _ => t :: acc
      }
      //(Conditional) special handling for nested applications: Don't save an intermediate result for each intermediate node.
      //Note that this is very syntactic, and that's good: if the user already inserted a binding for a partial application, as in:
      // val r1 = f arg1 arg2
      // val r2 = r1 arg3
      //we don't want to inline r1 into r2, because that might lead to work duplication.
      //Here, we assume that the user inserted all needed sharing already.
      val operands =
        if (partialApplicationsAreSpecial)
          collectApps(operator, operand :: Nil)
        else
          operator :: operand :: Nil
      aNormalizeName2(operands map (aNormalizeName(_)(bindings)) reduceLeft (App))(bindings)
    case Let(variable, exp, body) =>
      val normalExp = aNormalizeName(exp, Some(variable))(bindings)
      aNormalizeAddCaches(body, bindings)
    case v: Var =>
      (bindings.substs get v) getOrElse v
    case _ if isAtomic(t) => t
  }

  //From aNormalizeTerm
  def addCaches(t: Term): Term = {
    val bindings = createBindings()
    //XXX We need to also use adaptCallers.
    //And we need to use the full tuples when returning, and their first component in the rest of the computation.
    val normalT = aNormalizeAddCaches(t, bindings)
    val withAdaptedCallers = everywhere(adaptCallers)(normalT)
    def go(s: List[(Term, Var)], t: Term, vars: List[Var]): Term =
      s match {
        case Nil =>
          //This happens to return the result twice, if it is bound to a variable before return (that is, if it's not an atomic expression).
          //XXX Figure out the expected behavior and ensure it happens.
          //Answer: if the result is worth caching, it should indeed appear twice, as visible in the case for
          //user-defined functions in Fig. 3. They're assuming that results of primitives
          //needn't be cached and should be recomputed, which isn't the case for us.
          //XXX: HOWEVER! A-normal form allows an application as a result value, which isn't good for us.

          val transformedT = everywhere {
            orIdentity {
              case v: Var => transformVar(v)
            }
          }(t)
          ((transformedT :: (vars map transformVar)) foldLeft (tuple(vars.length + 1))) {
            _ ! _
          }
        case (term, variable) :: rest =>
          adaptCallers(Let(variable, term, go(rest, t, variable :: vars)))
      }
    go(bindings.bindings.toList, normalT, Nil)
  }

  //Should move to base.Names or base.Syntax.
  def transformName(transf: String => String): Name => Name = {
    def goNonIndexedName: NonIndexedName => NonIndexedName = {
      case DeltaName(n) => DeltaName(goNonIndexedName(n))
      case LiteralName(s) => LiteralName(transf(s))
    }
    def go: Name => Name = {
      case IndexedName(n, i) => IndexedName(goNonIndexedName(n), i)
      case nin: NonIndexedName => goNonIndexedName(nin)
      case _ => ???
    }
    go
  }

  def transformVar(varName: Name, fstT: Type): Var =
    Var(transformName(_ + "Tot")(varName), fstT)
  def transformVar(v: Var): Var = transformVar(v.getName, v.getType)

  //XXX: what about primitives? In the paper, you can distinguish them
  //statically, here you typically can't.
  def adaptCallers: Term => Term =
    orIdentity {
      //XXX Probably the type is not adapted yet, so we don't match against the ProductType
      case Let(variable @ Var(varName, fstT /*ProductType(fstT, sndT)*/), App(fun, arg), body) =>
        //XXX
        case class UnknownType() extends Type
        //No freshening for now. We'll need to thread more state for that.
        val varTot = /*fresh*/(transformVar(varName, ProductType(fstT, UnknownType())))
        Let(varTot, App(fun, arg),
          Let(variable, project(1) ! varTot, body))
    }
}
