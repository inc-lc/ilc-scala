package ilc
package feature
package base

/** Declares the abstract class Name and its subclasses.
  * Since subclasses of Name must have distinct string
  * representations, they are all declared here.
  */

import scala.language.implicitConversions

trait Names {
  sealed trait Name {
    override def toString: String = ???
  }

  implicit def stringAsLiteralName(string: String): Name =
    LiteralName(string)

  // NAME CLASSES IN ALPHABETIC ORDER

  // Delta names ends with 'D'
  case class DeltaName(original: Name) extends Name
  {
    override def toString: String =
      original.toString + 'D'
  }

  // Indexed names ends with """_\d+""".r
  case class IndexedName(original: Name, index: Int) extends Name
  {
    override def toString: String =
      original.toString + "_" + index
  }

  // Literal names ends with a lower-case letter.
  case class LiteralName(literal: String) extends Name {
    override def toString =
      if (literal matches """.*[a-z]\z""")
        literal
      else
        literal + "lit"
  }
}
