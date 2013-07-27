package ilc
package feature.functions

/**
 * Symbolic derivation for first-class functions.
 */
trait Derivation extends Syntax {
  // SUBCLASS OBLIGATIONS

  def deriveConst(c: Constant): Term

  // DERIVATION

  // String transformation
  def delta(x: String): String = "Δ" ++ x

  // Derivation follows Agda module
  // Syntax.Derive.Canon-Popl14

  def derive(t: Term): Term = t match {
    case Const(c)  => deriveConst(c)
    case Var(i)    => Var(2 * i)
    case Abs(x, t) => Abs(x, Abs(delta(x), derive(t)))
    case App(s, t) =>
      App(App(derive(s), weaken(_ * 2 + 1, t)), derive(t))
  }
}
