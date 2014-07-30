package ilc
package feature
package bintrees

trait Syntax
extends base.Syntax
   with Types
   with functions.Types
   with booleans.Types
{

  case object EmptyTree extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t => BinTreeType(t) }
  }

  case object Tree extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      BinTreeType(t) =>: t =>: BinTreeType(t) =>: BinTreeType(t)
    }
  }

  case object NodeValue extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      BinTreeType(t) =>: t
    }
  }

  case object LeftTree extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      BinTreeType(t) =>: BinTreeType(t)
    }
  }

  case object RightTree extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      BinTreeType(t) =>: BinTreeType(t)
    }
  }

  case object IsEmptyTree extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      BinTreeType(t) =>: BooleanType
    }
  }
}

trait InferenceSyntaxSugar extends Syntax with inference.SyntaxSugar {

  implicit class TreeOps[T <% UntypedTerm](t: T) {
    def nodeValue = asUntyped(NodeValue)(t)
    def leftTree = asUntyped(LeftTree)(t)
    def rightTree = asUntyped(RightTree)(t)
    def isEmptyTree = asUntyped(IsEmptyTree)(t)
  }

  def emptyTree = asUntyped(EmptyTree)
  def leaf(t: UntypedTerm) = tree(emptyTree, t, emptyTree)
  def tree(lhs: UntypedTerm, value: UntypedTerm, rhs: UntypedTerm)
    = asUntyped(Tree)(lhs, value, rhs)
}