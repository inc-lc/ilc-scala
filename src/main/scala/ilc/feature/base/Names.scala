package ilc
package feature
package base

/** Declares the abstract class Name and its subclasses.
  * Since subclasses of Name must have distinct string
  * representations, they are all declared here.
  */

import scala.language.implicitConversions

trait Names {
  /** The supertrait of all types of variable names.
    */
  sealed trait Name {
    override def toString: String = ???
  }

  sealed trait NonIndexedName extends Name

  implicit def stringAsLiteralName(string: String): NonIndexedName =
    LiteralName(string)

  // NAME CLASSES IN ALPHABETIC ORDER

  // Delta names end with 'D'
  case class DeltaName(original: NonIndexedName) extends NonIndexedName
  {
    override def toString: String =
      original.toString + 'D'
  }

  // Indexed names end with """_\d+""".r
  case class IndexedName(original: NonIndexedName, index: Int) extends Name
  {
    override def toString: String =
      original.toString + "_" + index
  }

  /** The variable names that a user writes:
    * x, y, z as opposed to dx, ddy, dddz
    */
  // Literal names end with a lower-case letter.
  case class LiteralName(literal: String) extends NonIndexedName {
    override def toString =
      if (literal matches """.*[a-z]\z""")
        literal
      else
        literal + "lit"
  }
}
