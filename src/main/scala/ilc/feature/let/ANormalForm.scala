package ilc
package feature
package let

import scalaz._

/**
 * Implementation of A-normalization, based on http://matt.might.net/articles/a-normalization/.
 * Written in CPS, so stack usage might be a problem. Should that ever happen, trampolining is an alternative.
 * Alternatively, one could implement this using a writer monad, as suggested by Tillmann.
 */
trait ANormalForm extends Syntax with FreeVariables with analysis.Occurrences with Traversals with ToScala with Pretty with IsAtomic {
  outer =>
  private val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  //XXX: Rename to avoid name clashes/confusion when importing (and also in ANormalFormStateful).
  def normalizeTerm(t: Term): Term = normalize(t)(identity)
  def normalize(t: Term)(k: Term => Term): Term = t match {
    case Abs(v, body) =>
      k(Abs(v, normalizeTerm(body)))
    case App(operator, operand) =>
      normalizeName(operator) { s =>
        normalizeName(operand) { t =>
          k(App(s, t))
        }
      }
    case Let(variable, exp, body) =>
      normalize(exp) { normalExp =>
        Let(variable, normalExp, normalize(body)(k))
      }
    case _ if isAtomic(t) => k(t)
  }

  def normalizeName(t: Term)(k: Term => Term) = {
    normalize(t) { normalT =>
      if (isAtomic(normalT))
        k(normalT)
      else {
        val newV = fresh("a", normalT.getType)
        Let(newV, normalT, k(newV))
      }
    }
  }
}

trait ANormalFormStateful extends Syntax with FreeVariables with analysis.Occurrences with Traversals with ToScala with Pretty with IsAtomic {
  outer =>
  private val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  import collection.mutable

  val doCSE = true
  val copyPropagation = true

  def normalizeTerm(t: Term): Term = {
    //Stores all bindings in order & without auto-removing duplicates.
    //So this works for both CSE and non-CSE.
    //In fact, we only need either bindings (for non-CSE) or a mutable map (for CSE).
    val bindings = mutable.ListBuffer.empty[(Term, Var)]
    val normalT = normalize(t)(bindings, mutable.Map.empty, mutable.Map.empty)
    bindings.foldRight(normalT) {
      case ((term, variable), t) =>
        Let(variable, term, t)
    }
  }

  /*
   * The only mutable state we use are the (global) fresh variable generator,
   * and the mutable maps threaded through as parameters, initialized by calls
   * to normalizeTerm (at the top-level and inside each lambda).
   */
  def normalize(t: Term)(bindings: mutable.ListBuffer[(Term, Var)], map: mutable.Map[Term, Var], substs: mutable.Map[Var, Term]): Term = t match {
    case Abs(v, body) =>
      Abs(v, normalizeTerm(body))
    case App(operator, operand) =>
      val s = normalizeName(operator)(bindings, map, substs)
      val t = normalizeName(operand)(bindings, map, substs)
      App(s, t)
    case Let(variable, exp, body) =>
      val normalExp = normalizeName(exp, Some(variable))(bindings, map, substs)
      normalize(body)(bindings, map, substs)
    case v: Var =>
      (substs get v) getOrElse v
    case _ if isAtomic(t) => t
  }

  def normalizeName(t: Term, boundVarOpt: Option[Var] = None)(bindings: mutable.ListBuffer[(Term, Var)], map: mutable.Map[Term, Var], substs: mutable.Map[Var, Term]): Term = {
    val normalT = normalize(t)(bindings, map, substs)
    def bind(): Var = {
      val newV = boundVarOpt getOrElse fresh("a", normalT.getType)
      map += normalT -> newV
      bindings += normalT -> newV
      newV
    }
    def reuse(existingTerm: Term): Term = {
      boundVarOpt foreach { boundVar =>
        //We can't just drop an existing binding.

        //This implements copy propagation.
        assert(isAtomic(normalT) && copyPropagation || doCSE) //Validate non-local reasoning: we only get here
        //if copyPropagation is enabled, because of the if below.
        substs += boundVar -> existingTerm
      }
      existingTerm
    }
    if (isAtomic(normalT) && (copyPropagation || boundVarOpt.isEmpty)) {
      reuse(normalT)
    } else {
      if (doCSE) {
        import std.option.optionSyntax._
        //Prevents calling bind() for a new binding of the same body.
        map get normalT some (reuse) none (bind())
      } else {
        bind()
      }
    }
  }
}
