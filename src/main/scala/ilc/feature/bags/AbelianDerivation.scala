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
   with abelianGroups.SyntaxSugar
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
      val BagType(elemType) = argumentTypes.head
      lambda(BagType(elemType)) { bag =>
        groupBasedChange ! FreeAbelianGroup(elemType) ! bag
      }
    }
  }

  private def changeTypes(valueType: Type): (Type, Type) =
    (groupBasedChangeType(BagType(valueType)), BagType(valueType))

  override def deriveSubterm(s: Subterm): Term = s.toTerm match {
    case EmptyBag(valueType) =>
      freeGroupBasedChange ! EmptyBag(valueType)

    case term @ Singleton(valueType) if s.hasStableArgument(0) =>
      lambdaDelta(term) { case Seq(v, dv) =>
        freeGroupBasedChange ! EmptyBag(valueType)
      }

    case term @ Union(valueType) => {
      val (grp, rep) = changeTypes(valueType)
      val freeAbelianGroup = FreeAbelianGroup(valueType)
      lambdaDelta(term) { case Seq(bag1, dbag1, bag2, dbag2) =>
        // CAUTION:
        // DO NOT bind the replacement change in a let-binding.
        // The object language is call-by-value.
        // Let-bindings evaluate the bound value eagerly.
        // Evaluating the replacement value eagerly cannot be
        // what we want.
        val replacement =
          super.deriveSubterm(s) ! bag1 ! dbag1 ! bag2 ! dbag2
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
        val (grp, rep) = changeTypes(valueType)
        val freeAbelianGroup = FreeAbelianGroup(valueType)
        val replacement =
          super.deriveSubterm(s) ! bag ! dbag
        case2(dbag,
          lambda(grp) { case grp0 =>
            ifThenElse(
              AreEqualGroups ! (Proj1 ! grp0) ! freeAbelianGroup,
              groupBasedChange ! freeAbelianGroup !
                (Negate ! (elementOfChange ! grp0)),
              replacement)
          },
          lambda(rep) { case _ => replacement })
      }

    case term @ FoldGroup(resultType, valueType)
        if isAbelianType(resultType) &&
           s.hasStableArgument(1) =>
      lambdaDelta(term) { case Seq(_G, dG, f, df, bag, dbag) =>
        val (grp, rep) = changeTypes(valueType)
        val freeAbelianGroup = FreeAbelianGroup(valueType)
        val replacement =
          super.deriveSubterm(s) ! _G ! dG ! f ! df ! bag ! dbag
        ifEqualGroups((_G, dG)) {
          case2(dbag,
            lambda(Var("grp0" ,grp)) { case grp0 =>
              ifEqualGroups(((Proj1 ! grp0), freeAbelianGroup)) {
                groupBasedChange ! _G !
                  (FoldGroup ! _G ! f ! (elementOfChange ! grp0))
              } {
                replacement
              }
            },
            lambda(rep) { case _ => replacement })
        } {
          replacement
        }
      }

    case _ =>
      super.deriveSubterm(s)
  }
}
