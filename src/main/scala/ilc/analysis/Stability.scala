package ilc
package analysis // package of static analyses

import ilc.feature.functions

trait Stability
extends functions.Context
   with FreeVariables
{

  case class Stability(
    ofVariables: Map[Variable, Boolean],
    ofArguments: Stream[Boolean]
  )


  object Stability {
    def entirelyUnstable: Stability =
      Stability(Map.empty, unknownArguments)

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
                Stability(pVarStability, Stability.unknownArguments)
            }
          }
        }
      }

    def isStableGiven(pVarStability: Map[Variable, Boolean]): Boolean =
      (subterm.freeVariables map pVarStability).fold(true)(_ && _)
  }
}
