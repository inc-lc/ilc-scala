package ilc
package feature
package products

/** Δ(σ × τ) = Δσ × Δτ
  *
  * This change type can support products of functions,
  * which is necessary to express groups.
  */

trait Derivation extends base.Derivation with Syntax {
  override def deltaType(tau: Type): Type = tau match {
    case ProductType(leftType, rightType) =>
      ProductType(deltaType(leftType), deltaType(rightType))

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case pType@ProductType(leftType, rightType) =>
      lambda(Var("dp", deltaType(pType)), Var("p", pType)) {
        case Seq(dp, p) =>
          Pair ! (updateTerm(leftType)  ! (Proj1 ! dp) ! (Proj1 ! p)) !
                 (updateTerm(rightType) ! (Proj2 ! dp) ! (Proj2 ! p))
      }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case pType@ProductType(leftType, rightType) =>
      lambda(Var("P", pType), Var("q", pType)) {
        case Seq(p, q) =>
          Pair ! (diffTerm(leftType)  ! (Proj1 ! p) ! (Proj1 ! q)) !
                 (diffTerm(rightType) ! (Proj2 ! p) ! (Proj2 ! q))
      }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case Pair(leftType, rightType) => {
      val (x, y) = (Var("x", leftType), Var("y", rightType))
      lambda(x, DVar(x), y, DVar(y)) { case Seq(x, dx, y, dy) =>
        Pair ! dx ! dy
      }
    }

    case Proj1(leftType, rightType) => {
      val p = Var("p", ProductType(leftType, rightType))
      lambda(p, DVar(p)) { case Seq(p, dp) =>
        Proj1 ! dp
      }
    }

    case Proj2(leftType, rightType) => {
      val p = Var("p", ProductType(leftType, rightType))
      lambda(p, DVar(p)) { case Seq(p, dp) =>
        Proj2 ! dp
      }
    }

    case _ =>
      super.derive(t)
  }
}
