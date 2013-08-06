package ilc
package feature.functions

/**
 * Stability of subterms
 * https://github.com/ps-mr/ilc/blob/master/stability.md
 */

trait Volatility
extends Attribution
   with FV { self: Syntax =>

  case class Volatility_attr(root: Term)
  extends InheritedAttribute[Boolean](root) {
    val rootAttr = ???
    def inherit(s: Subterm,
                childNumber: Int,
                parentAttr: Boolean) = ???
  } // TODO: revamp interface.

  type VolatilityEnv = Map[String, Boolean]

  case class VolatilityEnv_attr(root: Term)
  extends InheritedAttribute[(VolatilityEnv, Liability)](root) {
    // better safe than sorry:
    // assume all free variables to be volatile at root level
    // and assume nothing about future arguments
    val rootAttr = {
      val emptyEnv: VolatilityEnv = Map.empty
      (emptyEnv.withDefaultValue(true), NoInfo)
    }

    def inherit(s: Subterm,
                childNumber: Int,
                parentAttr: (VolatilityEnv, Liability)) = {
      //val (_, // TODO FIXME!!!
      ???
    }

    // cai 02.08.13
    // I'm not sure how to make `isNil` parametric.
    // the attributes it relies on is uncertain.
    def isNil(s: Subterm, env: VolatilityEnv): Boolean =
      ! isVolatile(s, env)

    def isVolatile(s: Subterm, env: VolatilityEnv): Boolean =
      FV.apply(s).map(env).fold(false)(_ || _)

    // free variables of all subterms
    val FV = FV_attr(root)
  }

  // data structure to record whether arguments are liable to change
  sealed trait Liability
  case object NoInfo extends Liability
  case class Yes(towardOthers: Liability) extends Liability
  case class No(towardOthers: Liability) extends Liability
}

object Volatility {
  //implicit def isNil(
}