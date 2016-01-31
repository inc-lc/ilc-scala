package ilc
package feature
package sums

/** Δ(a ⊎ b) = (Δa ⊎ Δb) ⊎ (a ⊎ b) */
trait ReplacementValuesDerivation
extends base.Derivation
   with Syntax
   with SyntaxSugar
{
  override def deltaType(tau: Type): Type = tau match {
    case SumType(a, b) =>
      SumType(SumType(deltaType(a), deltaType(b)), tau)

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case SumType(a, b) =>
      lambda(deltaType(tau), tau) { case Seq(deltaSum, oldSum) =>
        case2(deltaSum,
          lambda(SumType(deltaType(a), deltaType(b))) { surgery =>
            case4(surgery, oldSum,
              lambda(deltaType(a), a) { case Seq(dx, x) =>
                Inj1.tapply(b) ! (updateTerm(a) ! dx ! x)
              },
              // TODO: Introduce Error term and use it when
              // invalid changes are detected.
              lambda(deltaType(a), b) { case _ => oldSum }, // invalid
              lambda(deltaType(b), a) { case _ => oldSum }, // invalid
              lambda(deltaType(b), b) { case Seq(dy, y) =>
                Inj2.tapply(a) ! (updateTerm(b) ! dy ! y)
              })
          },
          lambda(tau) { replacement => replacement }
        )
      }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case SumType(a, b) =>
      lambda(tau, tau) { case Seq(newSum, oldSum) =>
        Inj2.tapply(SumType(deltaType(a), deltaType(b))) ! newSum
      }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case Inj1(leftType, rightType) =>
      lambda(leftType, deltaType(leftType)) { case Seq(x, dx) =>
        Inj1.tapply(SumType(leftType, rightType)) !
          (Inj1.tapply(deltaType(rightType)) ! dx)
      }

    case Inj2(leftType, rightType) =>
      lambda(rightType, deltaType(rightType)) { case Seq(y, dy) =>
        Inj1.tapply(SumType(leftType, rightType)) !
          (Inj1.tapply(deltaType(leftType)) ! dy)
      }

    // Either has a slow derivative
    case _ =>
      super.derive(t)
  }
}
