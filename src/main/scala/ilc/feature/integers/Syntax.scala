package ilc
package feature
package integers

import scala.language.implicitConversions


trait BaseSyntax
extends base.Syntax
   with Types
   with functions.Types
{
  case class LiteralInt(i: Int) extends Term {
    override lazy val getType: Type = IntType
  }
}

trait Syntax extends BaseSyntax {
  case object PlusInt extends Term {
    override lazy val getType: Type = IntType =>: IntType =>: IntType
  }

  case object NegateInt extends Term {
    override lazy val getType: Type = IntType =>: IntType
  }

  // ℤ can be folded over just like ℕ ⊎ ℕ.
  // foldInt isn't used yet. to add later?
}

trait SyntaxSugar
extends Syntax
   with abelianGroups.SyntaxSugar
{
  import Library._

  val additiveGroupOnIntegers: Term =
    AbelianGroup ! LiteralInt(additionIndexedGroupStamp) !
      PlusInt ! NegateInt ! LiteralInt(0)
}

trait ImplicitBaseSyntaxSugar extends BaseSyntax {
  implicit def intToTerm(n: Int): Term = LiteralInt(n)
}

trait ImplicitSyntaxSugar extends ImplicitBaseSyntaxSugar with SyntaxSugar
