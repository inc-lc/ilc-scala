package ilc
package feature
package abelianGroups

trait Syntax extends base.Syntax with Types with booleans.Types {
  case object AbelianGroup extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      (e =>: e =>: e) =>: (e =>: e) =>: e =>: GroupType(e)
    }
  }

  case object GetBinOp extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      GroupType(e) =>: binOpType(e)
    }
  }

  case object GetInv extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      GroupType(e) =>: invType(e)
    }
  }

  case object GetNeutral extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      GroupType(e) =>: e
    }
  }

  case object AreEqualGroups extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      GroupType(e) =>: GroupType(e) =>: BoolType
    }
  }
}

trait SyntaxSugar extends Syntax {
  //def groupUnfoldTerm: TermBuilder = GroupUnfold
  // stability framework can't tell higher-order arguments if
  // their arguments are stable. `groupUnfold` has to be a
  // metafunction.
}
