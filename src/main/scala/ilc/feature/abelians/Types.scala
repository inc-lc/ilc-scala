package ilc
package feature
package abelians

/** Abelian groups in the object language
  *
  * For collection types τ, we have
  *
  * Δτ = Abelian τ × τ
  *
  * It falls to the user to verify that an inhabitant of
  * Abelian τ actually forms a group, and that the mapping
  * function supplied to a foldGroup is a homomorphism.
  */

trait Types
extends products.SyntaxSugar
{
  /** Abelian groups are encoded as products
    * so as to take advantage of Δ(σ × τ) = Δσ × Δτ
    */
  def abelianType(elementType: Type): Type = {
    val tau = elementType
    tupleType(tau =>: tau =>: tau, tau =>: tau, tau)
  }
}
