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

  case object NegateInt extends Term {
    override def getType: Type = IntType =>: IntType
  }

  // ℤ can be folded over just like ℕ ⊎ ℕ.
  // foldInt isn't used yet. to add later?
}

trait SyntaxSugar
extends Syntax
   with abelianGroups.SyntaxSugar
   with functions.SyntaxSugar
{
  // code for addition = big-endian encoding of "add!"

  private val additionCode : Int = {
    val bytes = Array('a', 'd', 'd', '!').map(_.toInt)
    (0 until bytes.length).map(i =>
      bytes(i) << 8*(bytes.length - i - 1)
    ).sum
  }

  assert(additionCode == 0x61646421)

  val additiveGroupOnIntegers: Term =
    AbelianGroup ! LiteralInt(additionCode) !
      PlusInt ! NegateInt ! LiteralInt(0)
}
