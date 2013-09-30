package ilc
package feature
package integers

import scala.language.implicitConversions

trait Syntax
extends base.Syntax
   with Types
   with functions.Types
{
  case class Literal(i: Int) extends Term {
    override def getType: Type = IntType
  }

  case object Plus extends Term {
    override def getType: Type = IntType =>: IntType =>: IntType
  }

  case object Minus extends Term {
    override def getType: Type = IntType =>: IntType =>: IntType
  }

  // ℤ can be folded over just like ℕ ⊎ ℕ.
  // foldInt isn't used yet. to add later?

  implicit def intToTerm
    (i: Int): Term = Literal(i)
}
