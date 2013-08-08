package ilc
package feature.functions

/**
 * Stability of subterms
 * https://github.com/ps-mr/ilc/blob/master/stability.md
 */

trait Stability
extends Attribution
   with FV { self: Syntax =>

  type VarStability = Map[String, Boolean]
  type ArgStability = List[Boolean]

  // pattern-matching helper object
  // assume future arguments to be unstable when nothing is known
  // about them.
  object ArgStability {
    def unapply(arg: ArgStability): Option[(Boolean, ArgStability)] =
      arg match {
        case firstArgIsStable :: theRest =>
          Some(firstArgIsStable -> theRest)
        case Nil =>
          Some(false -> Nil)
      }
  }

  case class SubtermStability(attr: Stability_attr)
  extends ReadOnlyAttribute[Boolean] {
    def lookup(s: Subterm): Boolean = attr.isStable(s)
  }

  case class ArgumentStability(attr: Stability_attr)
  extends ReadOnlyAttribute[Int => Array[Boolean]] {

    def apply(s: Subterm, arity: Int): Array[Boolean] =
      lookup(s)(arity)

    def lookup(s: Subterm): Int => Array[Boolean] = n => {
      val argStability: Array[Boolean] = attr(s)._2.toArray
      val returnValue: Array[Boolean] = Range(0, n).map(
        i => i < argStability.length && argStability(i)
      )(scala.collection.breakOut)
      assert(returnValue.size == n)
      returnValue
    }
  }

  case class Stability_attr(root: Term)
  extends InheritedAttribute[(VarStability, ArgStability)](root) {

    // factory method to make easier-to-use attributes
    def split: (SubtermStability, ArgumentStability) =
      (SubtermStability(this), ArgumentStability(this))

    val rootAttr = {
      val emptyEnv: VarStability = Map.empty
      (emptyEnv.withDefaultValue(false), Nil)
    }

    def inherit(parent: Subterm,
                parentAttr: (VarStability, ArgStability)) = {
      val (env, arg) = parentAttr
      (parent.term, arg) match {
        case (Abs(x, _), ArgStability(firstArg, otherArgs)) =>
          List(env.updated(x, firstArg) -> otherArgs)
        case (App(t0, t1), _) =>
          List(env -> (isStable(t1, env) :: arg),
               env -> Nil)
      }
    }

    def isStable(s: Subterm): Boolean =
      isStable(s.term, lookup(s)._1)

    protected def isStable(t: Term, env: VarStability): Boolean =
      FV(t).map(env).fold(true)(_ && _)

    // free variables of all subterms
    val FV = FV_attr(root)
  }
}
