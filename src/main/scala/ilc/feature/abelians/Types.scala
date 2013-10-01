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
extends base.TypeConstructor
   with products.Types
{
  /** (de-)constructor of the type of abelian groups */
  object AbelianType {
    def apply(elementType: Type): Type = {
      val tau = elementType
      tupleType(tau =>: tau =>: tau, tau =>: tau, tau)
    }

    def unapply(abelianType: Type): Option[Type] = {
      val tuple = tupleTypeExtractor(3)
      abelianType match {
        case tuple(Seq(a0 =>: a1 =>: a2, a3 =>: a4, a5))
            if List(a0, a1, a2, a3, a4, a5).distinct.size == 1 =>
          Some(a0)

        case _ =>
          None
      }
    }
  }
}
