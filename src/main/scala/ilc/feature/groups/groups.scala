package ilc
package feature
package groups

trait Types extends functions.Types {
  case class GroupType(elType: Type) extends Type

  def binOpType(e: Type) = e =>: e =>: e
  def invType(e: Type) = e =>: e
}

trait Syntax extends base.Syntax with Types {
  case object GroupConstructor extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("e") { e =>
      (e =>: e =>: e) =>: (e =>: e) =>: e =>: GroupType(e)
    }
  }

  case object GroupUnfold extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("e", "r") { case Seq(e, r) =>
      GroupType(e) =>:
      (GroupType(e) =>: (e =>: e =>: e) =>: (e =>: e) =>: e =>: r) =>:
      r
    }
  }
}

trait SyntaxSugar extends Syntax {
  def groupBuildTerm: TermBuilder = GroupConstructor
  def groupUnfoldTerm: TermBuilder = GroupUnfold
}