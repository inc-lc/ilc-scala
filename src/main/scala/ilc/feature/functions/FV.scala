package ilc
package feature.functions

/**
 * Free variables
 */

trait FV extends Syntax {
  def FV(t: Term): Set[String] = t match {
    case Const(constant) =>
      Set.empty

    case Var(name) =>
      Set(name)

    case App(operator, operand) =>
      FV(operator) ++ FV(operand)

    case Abs(name, body) =>
      FV(body) - name
  }
}

