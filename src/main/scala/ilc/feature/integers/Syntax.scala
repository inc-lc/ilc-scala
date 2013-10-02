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
    override def getType: Type = IntType
  }

  case object PlusInt extends Term {
    override def getType: Type = IntType =>: IntType =>: IntType
  }

  case object MinusInt extends Term {
    override def getType: Type = IntType =>: IntType =>: IntType
  }

  // ℤ can be folded over just like ℕ ⊎ ℕ.
  // foldInt isn't used yet. to add later?
}
