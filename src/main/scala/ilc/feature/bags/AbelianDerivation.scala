package ilc
package feature
package bags

/** {{{
  * Δ(Bag a) = (AbelianGroup (Bag a) × Bag a) ⊎ Bag a
  * }}}
  */

trait AbelianDerivation
extends Syntax
   with analysis.Stability
   with functions.SyntaxSugar // for let-bindings
   with booleans.SyntaxSugar  // for ifThenElse, andTerm
   with abelianGroups.AbelianDerivation
   with functions.ContextSensitiveDerivation
{
  override def isAbelianType(tau: Type): Boolean = tau match {
    case BagType(_) => true
    case _          => super.isAbelianType(tau)
  }

  def freeGroupBasedChange: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term = {
      val elemType = argumentTypes.head
      lambda(elemType) { element =>
        groupBasedChange ! FreeAbelianGroup(elemType) ! element
      }
    }
  }

  override def deriveSubterm(s: Subterm): Term = s.toTerm match {
    case EmptyBag(valueType) =>
      freeGroupBasedChange ! EmptyBag(valueType)

    case Singleton(valueType) if s.hasStableArgument(0) =>
      freeGroupBasedChange ! EmptyBag(valueType)

    case term @ Union(valueType) => {
      val (grp, rep) =
        (groupBasedChangeType(BagType(valueType)), BagType(valueType))
      val freeAbelianGroup = FreeAbelianGroup(valueType)
      lambdaDelta(term) { case Seq(bag1, dbag1, bag2, dbag2) =>
        // CAUTION:
        // DO NOT bind the replacement change in a let-binding.
        // The object language is call-by-value.
        // Let-bindings evaluate the bound value eagerly.
        // Evaluating the replacement value eagerly cannot be
        // what we want.
        val replacement: TermBuilder =
          replacementChange !
            (Union ! (ChangeUpdate ! dbag1 ! bag1) !
                     (ChangeUpdate ! dbag2 ! bag2))
        case4(dbag1, dbag2,
          lambda(grp, grp) { case Seq(grp1, grp2) =>
            ifThenElse(
              andTerm !
                (AreEqualGroups ! (Proj1 ! grp1) ! (Proj1 ! grp2)) !
                (AreEqualGroups ! (Proj1 ! grp1) ! freeAbelianGroup),
              groupBasedChange ! freeAbelianGroup !
                (Union ! (elementOfChange ! grp1) !
                  (elementOfChange ! grp2)),
              replacement)
          },
          lambda(grp, rep) { case _ => replacement },
          lambda(rep, grp) { case _ => replacement },
          lambda(rep, rep) { case _ => replacement })
      }
    }

    case term @ Negate(valueType) =>
      lambdaDelta(term) { case Seq(bag, dbag) =>
        // TODO: make sure dbag has bag group!
        ??? // Negate ! dbag
      }

    case term @ FoldGroup(resultType, valueType)
        if isAbelianType(resultType) &&
           s.hasStableArgument(0) && s.hasStableArgument(1) =>
      lambdaDelta(term) { case Seq(_G, dG, f, df, bag, dbag) =>
        ???
      }

    case _ =>
      super.deriveSubterm(s)
  }
}
