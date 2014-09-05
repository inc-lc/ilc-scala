package ilc
package analysis // package of static analyses

import ilc.feature.functions

/**
  * Assume you're interested in deriving a closed function f.
  * You want to know how the result changes if the input arguments of f change.
  * By extension, you want to know how subterms of f change when the inputs of f change.
  * Some subterms will never change. We say these subterms are *stable*.
  * Stability analysis figures that out.
  *
  * @author Cai Yufei
  * @author Paolo Giarrusso for extra documentation (adapted from Cai's stability.md)
  */
trait Stability
extends functions.Context
   with FreeVariables
{
  /**
    * This represents the internal state of stability analysis — its context.
    * To compute the stability of a term, we are interested in knowing:
    * - whether its arguments are stable.
    * - which variables are stable.
    */
  case class Stability(
    ofVariables: Map[Var, Boolean],
    ofArguments: Stream[Boolean]
  )


  object Stability {
    /**
      * This is the stability of the program f. This value asserts that we know
      * nothing about the stability of arguments, and that all arguments are
      * unstable.
      *
      * This does not mean that f itself is unstable. In fact, f itself is
      * always stable, and its body might be stable if f is a constant function.
      */
    def entirelyUnstable: Stability =
      Stability(Map.empty.withDefaultValue(false), unknownArguments)

    def unknownArguments: Stream[Boolean] =
      Stream.continually(false)
  }

  implicit class StabilityOfSubterms(subterm: Subterm) {
    def isStable: Boolean =
      this.isStableGiven(this.getStability.ofVariables)

    def hasStableArgument(i: Int): Boolean =
      this.getStability.ofArguments(i)

    def getStability: Stability =
      if (subterm.isRoot)
        Stability.entirelyUnstable
      else {
        val parent = subterm.parent
        val Stability(pVarStability, pArgStability) = parent.getStability
        parent.toTerm match {
          case Abs(x, _) =>
            Stability(pVarStability.updated(x, pArgStability.head),
                      pArgStability.tail)

          case App(_, _) => {
            val Seq(operator, operand) = parent.children
            subterm.siblingOrdinalPosition match {
              case 0 =>
                Stability(pVarStability,
                          operand.isStableGiven(pVarStability) #::
                            pArgStability)

              case 1 =>
                //For arguments, we punt, and just say that we don't know if their arguments are stable or not.
                Stability(pVarStability, Stability.unknownArguments)
            }
          }
        }
      }

    // A term is stable if all its free variables are stable.
    def isStableGiven(pVarStability: Map[Var, Boolean]): Boolean =
      (subterm.freeVariables map pVarStability).fold(true)(_ && _)
  }
}
