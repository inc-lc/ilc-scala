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
  extends ReadOnlyAttribute[Int => Boolean] {

    def apply(s: Subterm, whichArgument: Int): Boolean =
      lookup(s)(whichArgument)

    def lookup(s: Subterm): Int => Boolean = n => {
      def seek(i: Int, list: List[Boolean]): Boolean = list match {
        case Nil => false
        case bool :: _ if (i == 0) => bool
        case _ :: rest if (i >  0) => seek(i - 1, rest)
      }
      seek(n, attr(s)._2)
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
