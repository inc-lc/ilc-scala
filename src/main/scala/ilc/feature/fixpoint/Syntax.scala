package ilc
package feature
package fixpoint

trait Syntax extends base.Syntax with functions.Types {
  case object Fix extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("t") { t =>
      (t =>: t) =>: t
    }
  }
}
