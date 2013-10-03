package ilc
package feature
package functions

/**
 * Symbolic derivation for first-class functions.
 */
trait Derivation
extends LambdaDelta
{
  override def deltaType(tau: Type): Type = tau match {
    case domain =>: range =>
      domain =>: deltaType(domain) =>: deltaType(range)

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case functionType@(domain =>: range) =>
      lambda(
        Var("df", deltaType(functionType)),
        Var("f", functionType),
        Var("x", domain)
      ) {
        case Seq(df, f, x) =>
          updateTerm(range) !
            (df ! x ! (diffTerm(domain) ! x ! x)) !
            (f ! x)
      }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case functionType@(domain =>: range) =>
      lambda(
        Var("g", functionType),
        Var("f", functionType),
        Var("x", domain),
        Var("dx", deltaType(domain))
      ) {
        case Seq(g, f, x, dx) =>
          diffTerm(range) !
            (g ! (updateTerm(domain) ! dx ! x)) !
            (f ! x)
      }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case Abs(x, body) =>
      lambdaTerm(x, DVar(x)) { derive(body) }

    case App(operator, operand) =>
      derive(operator) ! operand ! derive(operand)

    case _ =>
      super.derive(t)
  }
}
