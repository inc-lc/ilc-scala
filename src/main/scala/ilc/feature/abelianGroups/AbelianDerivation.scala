package ilc
package feature
package abelianGroups

import java.lang.Class

/** a type τ is an "abelian type" if
  * {{{
  * Δτ = (AbelianGroup τ × τ) ⊎ τ
  * }}}
  */

trait AbelianDerivation
extends Derivation
   with products.Syntax
   with sums.SyntaxSugar
{
  /** subclasses must declare their types to be "abelian"
    * to take advantage of the services herein.
    */
  def isAbelianType(tau: Type): Boolean = false

  override def deltaType(tau: Type): Type =
    if (isAbelianType(tau))
      SumType(groupBasedChangeType(tau), tau)
    else
      super.deltaType(tau)

  /** first half of the delta type of an abelian type:
    * the group and an element of it.
    */
  def groupBasedChangeType(tau: Type): Type =
    ProductType(AbelianGroupType(tau), tau)

  def groupBasedChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case groupType @ AbelianGroupType(elemType) =>
          lambda(Var("group", groupType), Var("elem", elemType)) {
            case Seq(group, elem) =>
              Inj1(groupBasedChangeType(elemType), elemType) ! (Pair ! group ! elem)
          }
      }
  }

  def replacementChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term = {
      val tau = argumentTypes.head
      Inj2(groupBasedChangeType(tau), tau)
    }
  }

  /** binary operator of a group-based change */
  def binOpOfChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case pType @ ProductType(AbelianGroupType(elemType), elemType2)
            if elemType == elemType2 =>
          GetBinOp composeWith Proj1%pType
      }
  }

  /** inverse function of a group-based change */
  def invOfChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case pType @ ProductType(AbelianGroupType(elemType), elemType2)
            if elemType == elemType2 =>
          GetInv composeWith Proj1%pType
      }
  }

  /** neutral element of a group-based change */
  def neutralOfChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case pType @ ProductType(AbelianGroupType(elemType), elemType2)
            if elemType == elemType2 =>
          GetNeutral composeWith Proj1%pType
      }
  }

  /** the group element that represents a group-based change */
  def elementOfChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case pType @ ProductType(AbelianGroupType(elemType), elemType2)
            if elemType == elemType2 =>
          Proj2%pType
      }
  }

  override def updateTerm(tau: Type): Term =
    if (isAbelianType(tau))
      lambda(Var("dn", deltaType(tau)), Var("n", tau)) {
        case Seq(dn, n) =>
          case2(dn,
            // group element
            lambda(groupBasedChangeType(tau)) { elem =>
              binOpOfChange ! elem ! (elementOfChange ! elem) ! n
            },
            // replacement
            lambda(Var("replacement", tau)) {x => x})
      }
    else
      super.updateTerm(tau)

  // return replacement always
  override def diffTerm(tau: Type): Term =
    if (isAbelianType(tau))
      lambda(tau, tau) { case Seq(m, n) => replacementChange ! m }
    else
      super.diffTerm(tau)
}
