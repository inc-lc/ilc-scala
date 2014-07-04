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

  override def aNormalizeTerm(t: Term): Term = {
    val bindings = if (doCSE) new CSEBindings else new NonCSEBindings
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
      operands map (aNormalizeName(_)(bindings)) reduceLeft (App)
    case Let(variable, exp, body) =>
      val normalExp = aNormalizeName(exp, Some(variable))(bindings)
      aNormalize(body, bindings)
    case v: Var =>
      (bindings.substs get v) getOrElse v
    case _ if isAtomic(t) => t
  }

  def aNormalizeName(t: Term, boundVarOpt: Option[Var] = None)(bindings: Bindings): Term = {
    val normalT = aNormalize(t, bindings)
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
