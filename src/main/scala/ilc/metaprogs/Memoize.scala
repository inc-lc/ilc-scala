package ilc
package metaprogs

/*
 * This file contains protoytype code related to memoization, both the term
 * transformation and the library support code.
 */

import feature._
import collection.mutable

trait Memoize extends memoize.MemoizeBase with let.IsAtomic {
  //In fact, we should get the output of CSE probably, so we have good reasons to support let.
  this: ilc.feature.functions.Syntax with let.Syntax with base.Derivation with memoize.Syntax with analysis.FreeVariables with base.ToScala =>

  class MemoContext(cacheMap: mutable.Map[Term, CacheEntry] = mutable.Map[Term, CacheEntry]()) extends MemoContextBase(cacheMap) {
    def memoIfPossible(t: Term): Term = cacheMap get t match {
      case Some(entry) => Memo(entry, updateCache = false) ! t
      case None => t
    }

    def memoizedDerive(t: Term): Term = t match {
      // This case is as needed as `let.Derivation`, see there for discussion.
      case Let(x, term, body) =>
        Let(x, memoIfPossible(term),
          Let(DVar(x), memoizedDerive(term),
            memoizedDerive(body)))
      case Abs(x, body) =>
        lambdaTerm(x, DVar(x)) { memoizedDerive(body) }

      case App(operator, operand) =>
        memoizedDerive(operator) !
          //operand ! // In non-memoizing derivation
          memoIfPossible(operand) ! // Main (only) change from non-memoizing derivation!
          memoizedDerive(operand)

      case v: Var =>
        DVar(v)

      // For all terms we don't know how to derive,
      // we produce a derivative that does recomputation.
      // This makes adding new constants easy.
      case _ =>
        Diff ! t ! t
    }

    def transform(t: Term): Term = {
      def memoNode = Memo(getOrElseNewCacheEntry(t), updateCache = true)
      t match {
        //For atoms (variables and constants), do *no* memoization.
        case x if isAtomic(x) => x
        case Let(x, term, body) =>
          memoNode !
            Let(x, transform(term), transform(body))
        case App(s, t) =>
          memoNode !
            App(transform(s), transform(t))
        case Abs(x, t) =>
          //XXX This will memoize each function in a chain of nested lambdas.
          // But this can only be solved through CBPV.
          memoNode !
            Abs(x, transform(t))
        //case x: Var => x
      }
    }

  }
}
