package ilc
package feature
package abelianGroups

trait Syntax
extends base.Syntax
   with Types
   with booleans.Types
   with integers.Types // for identifying abelian groups
{
  case object AbelianGroup extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      IntType =>:
        (e =>: e =>: e) =>: (e =>: e) =>: e =>: AbelianGroupType(e)
    }
  }

  case object GetBinOp extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      AbelianGroupType(e) =>: binOpType(e)
    }
  }

  case object GetInv extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      AbelianGroupType(e) =>: invType(e)
    }
  }

  case object GetNeutral extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      AbelianGroupType(e) =>: e
    }
  }

  case object AreEqualGroups extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      AbelianGroupType(e) =>: AbelianGroupType(e) =>: BooleanType
    }
  }
}

trait SyntaxSugar
extends Syntax
   with booleans.SyntaxSugar
{
  def ifEqualGroups(firstPair: (TermBuilder, TermBuilder),
                    groupPairs: (TermBuilder, TermBuilder)*)
                   (thenBranch: => TermBuilder)
                   (elseBranch: => TermBuilder): TermBuilder =
    ifThenElse(
      AreEqualGroups ! firstPair._1 ! firstPair._2,
      (if (groupPairs.isEmpty)
        thenBranch
      else
        ifEqualGroups(groupPairs.head, groupPairs.tail: _*)(
          thenBranch)(elseBranch)),
      elseBranch)

  //def groupUnfoldTerm: TermBuilder = GroupUnfold
  // stability framework can't tell higher-order arguments if
  // their arguments are stable. `groupUnfold` has to be a
  // metafunction.
}
