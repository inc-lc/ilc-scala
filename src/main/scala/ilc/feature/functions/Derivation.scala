package ilc
package feature.functions

/**
 * Symbolic derivation for first-class functions.
 */
trait Derivation extends Syntax with FV {
  // SUBCLASS OBLIGATIONS

  def deriveConst(c: Constant): Term

  // DERIVATION

  // String transformation
  def delta(x: String): String = "Δ" ++ x

  // Derivation follows Agda module
  // Syntax.Derive.Canon-Popl14

  def derive(t: Term): Term = t match {
    case Const(constant) =>
      deriveConst(constant)

    case Var(name) =>
      Var(delta(name))

    case App(operator, operand) =>
      App(App(derive(operator), operand), derive(operand))

    case Abs(x, body) => {
      val dx = delta(x)
      if (FV(t) contains dx)
        sys.error("naming scheme violation " ++
          "when deriving:\n  " ++ t.toString)
      else
        Abs(x, Abs(dx, derive(body)))
    }
  }
}
