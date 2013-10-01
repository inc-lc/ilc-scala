package ilc
package feature
package integers

/** Δℤ = (Abelian ℤ × ℤ) ⊎ ℤ
  *
  * The change to an integer is an abelian group element or a
  * replacement.
  */
trait AbelianDerivation
extends base.Derivation
   with Syntax
   with functions.Derivation // for `lambdaDelta`
   with abelians.Derivation
   with sums.SyntaxSugar
{
  private def groupElemType = ProductType(AbelianType(IntType), IntType)
  private def getBinaryOp = binaryOp composeWith Proj1%groupElemType
  private def getNegation = negation composeWith Proj1%groupElemType
  private def getNeutral  = neutral  composeWith Proj1%groupElemType
  private def getElement  = Proj2%groupElemType
  private def replaceBy   = Inj2(groupElemType, IntType)

  override def deltaType(tau: Type): Type = tau match {
    case IntType =>
      SumType(groupElemType, IntType)

    case _ =>
      super.deltaType(tau)
  }

  override def updateTerm(tau: Type): Term = tau match {
    case IntType => {
      lambda(Var("dn", deltaType(IntType)), Var("n", IntType)) {
        case Seq(dn, n) =>
          case2(dn,
            // group element
            lambda(groupElemType) { groupElem =>
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
    case Literal(i) =>
      replaceBy ! i

    // Plus and Minus can use the slow derivative. It operates on
    // integers, so calling `updateTerm` and `diffTerm` is okay.

    case _ =>
      super.derive(t)
  }
}
