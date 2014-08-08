package ilc
package feature
package abelianMaps

/** Δ(Map k v) = (AbelianGroup (Map k v) × Map k v) ⊎ Map k v
  *
  * The change to a map is an abelian group element or a
  * replacement.
  */
trait AbelianDerivation
extends Syntax
   with abelianGroups.SyntaxSugar
   with booleans.SyntaxSugar
   with abelianGroups.Derivation
   with abelianGroups.AbelianDerivation
   with functions.ContextSensitiveDerivation
   with analysis.Stability
{
  override def isAbelianType(tau: Type): Boolean = tau match {
    case MapType(_, _) =>
      true

    case _ =>
      super.isAbelianType(tau)
  }

  override def deriveSubterm(s: Subterm): Term = s.toTerm match {
    case term @ FoldByHom(k, a, b)
        if isAbelianType(b) &&
           s.hasStableArgument(2) => // f : k → a → b
      lambdaDelta(term) { case Seq(_Ga, dGa, _Gb, dGb, f, df, m, dm) =>
        val grp = ProductType(AbelianGroupType(MapType(k, a)), MapType(k, a))
        val rep = MapType(k, a)
        val replacement: TermBuilder =
          super.deriveSubterm(s) ! _Ga ! dGa ! _Gb ! dGb ! f ! df ! m ! dm

        case2(dm,
          lambda(grp) { case grp0 =>
            ifEqualGroups((Proj1 ! grp0, LiftGroup(k) ! _Ga),
                          (_Ga, dGa),
                          (_Gb, dGb)) {
              groupBasedChange ! _Gb !
              (FoldByHom ! _Ga ! _Gb ! f ! (elementOfChange ! grp0))
            } {
              replacement
            }
          },
          lambda(rep) { _ => replacement })
      }

    case _ =>
      super.deriveSubterm(s)
  }
}
