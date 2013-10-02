package ilc
package feature
package integers

/** Δℤ = (AbelianGroup ℤ × ℤ) ⊎ ℤ
  *
  * The change to an integer is an abelian group element or a
  * replacement.
  */
trait AbelianDerivation
extends base.Derivation
   with Syntax
   with functions.Derivation // for `lambdaDelta`
   with abelianGroups.Derivation
   with products.Syntax
   with sums.SyntaxSugar
{
  private def deltaInt    = ProductType(AbelianGroupType(IntType), IntType)
  private def getBinaryOp = GetBinOp   composeWith Proj1%deltaInt
  private def getInverse  = GetInv     composeWith Proj1%deltaInt
  private def getNeutral  = GetNeutral composeWith Proj1%deltaInt
  private def getElement  = Proj2%deltaInt
  private def replaceBy   = Inj2(deltaInt, IntType)

  override def deltaType(tau: Type): Type = tau match {
    case IntType =>
      SumType(deltaInt, IntType)

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case IntType => {
      lambda(Var("dn", deltaType(IntType)), Var("n", IntType)) {
        case Seq(dn, n) =>
          case2(dn,
            // group element
            lambda(deltaInt) { groupElem =>
              getBinaryOp ! groupElem ! (getElement ! groupElem) ! n
            },
            // replacement
            lambda(Var("replacement", IntType)) {x => x})
      }
    }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case IntType =>
      // return replacement always
      lambda(IntType, IntType) { case Seq(m, n) =>
        replaceBy ! m
      }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case LiteralInt(i) =>
      replaceBy ! t

    // PlusInt and NegateInt can use the slow derivative. It operates on
    // integers, so calling `updateTerm` and `diffTerm` is okay.

    case _ =>
      super.derive(t)
  }
}
