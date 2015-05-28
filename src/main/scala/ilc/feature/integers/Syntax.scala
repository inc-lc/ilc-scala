package ilc
package feature
package integers

import scala.language.implicitConversions

trait Syntax
extends base.Syntax
   with Types
   with functions.Types
{
  case class LiteralInt(i: Int) extends Term {
    override lazy val getType: Type = IntType
  }

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

trait ImplicitSyntaxSugar
extends SyntaxSugar
{
  implicit def intToTerm(n: Int): Term = LiteralInt(n)
}
