package ilc
package metaprogs

/*
 * This file contains protoytype code related to memoization, both the term
 * transformation and the library support code.
 */

import feature._
import collection.mutable

trait Memoize extends memoize.MemoizeBase {
  //In fact, we should get the output of CSE probably, so we have good reasons to support let.
  outer: ilc.feature.functions.Syntax with let.Syntax with base.Derivation with memoize.Syntax with analysis.FreeVariables with base.ToScala =>

  class MemoContext(cacheMap: mutable.Map[Term, CacheEntry] = mutable.Map[Term, CacheEntry]()) extends MemoContextBase(cacheMap) {
  def memoizedDerive(t: Term): Term = t match {
    case Abs(x, body) =>
      lambdaTerm(x, DVar(x)) { memoizedDerive(body) }

    case App(operator, operand) =>
      //XXX: What happens here if we have two instances of the same term around?
      val memoizedOperand: Term = Memo(cacheMap(operand), updateCache = false) ! operand
      memoizedDerive(operator) !
        //operand ! // In non-memoizing derivation
        memoizedOperand ! // Main (only) change from non-memoizing derivation!
        memoizedDerive(operand)

    case v: Var =>
      DVar(v)

    // For all terms we don't know how to derive,
    // we produce a derivative that does recomputation.
    // This makes adding new constants easy.
    case _ =>
      Diff ! t ! t
  }

  def doTransform(t: Term, freeVars: List[Var], m: Map[Term, Name] = Map.empty): Term = {
    val cacheEntry = getOrElseNewCacheEntry(t)

    /*
    val memoizedSubterms: Term = t match {
      case App(s, t) => App(doTransform(s, freeVars), doTransform(t, freeVars))
      case Abs(x, t) => Abs(x, doTransform(t, x :: freeVars))
      //case x: Var => x
      case x => x
    }

    Memo(cacheEntry) ! memoizedSubterms
    */
    def memoNode = Memo(cacheEntry, updateCache = true)
    t match {
      case Let(x, term, body) =>
        memoNode !
          Let(x, doTransform(term, freeVars), doTransform(body, freeVars))
      case App(s, t) =>
        memoNode !
          App(doTransform(s, freeVars), doTransform(t, freeVars))
      case Abs(x, t) =>
        //XXX This will memoize each function in a chain of nested lambdas.
        // But this can only be solved through CBPV.
        memoNode !
          Abs(x, doTransform(t, x :: freeVars))
      //case x: Var => x
      //Otherwise, for atoms (variables and constants), do *no* memoization.
      case x => x
    }

  }

  def transform(t: Term) = doTransform(t, List())
  }
}
