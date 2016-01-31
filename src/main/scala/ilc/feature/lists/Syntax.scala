package ilc
package feature
package lists

trait Syntax
extends base.Syntax
   with Types
   with functions.Types
   with booleans.Types
{

  case object Empty extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t => ListType(t) }
  }

  case object Cons extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      t =>: ListType(t) =>: ListType(t)
    }
  }

  case object Head extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      ListType(t) =>: t
    }
  }

  case object Tail extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      ListType(t) =>: ListType(t)
    }
  }

  case object IsEmpty extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("elemType") { t =>
      ListType(t) =>: BooleanType
    }
  }
}

trait InferenceSyntaxSugar extends Syntax with inference.SyntaxSugar {

  implicit class ListOps[T <% UntypedTerm](t: T) {
    def :::(s: UntypedTerm) = Cons(s, t)
    def head = Head(t)
    def tail = Tail(t)
    def isEmpty = IsEmpty(t)
    def get(i: Int) = project(i, t)
  }

  def emptyList: UntypedTerm = Empty

  def list(args: UntypedTerm*) =
    args.foldRight(emptyList)(_ ::: _)

  //i is 0-based.
  def project(i: Int, t: UntypedTerm): UntypedTerm =
    if (i < 0)
      sys error s"${i}-th tuple projections are not supported (tuple indexes start from 0)"
    else if (i == 0)
      t.head
    else
      project(i - 1, t.tail)
}