package ilc
package feature
package functions

import scala.language.postfixOps

trait ToScala extends base.ToScala with TypedSyntax {
  override def toScala(t: Term): String = t match {
    case App(f, x) =>
      "%s(%s)".format(toScala(f), toScala(x))

    case Var(name) =>
      toScalaIdentifier(name)

    case TypedAbs(name, argType, bodyTerm) => {
      val x = toScalaIdentifier(name)
      val xType = toScala(argType)
      val body = toScala(bodyTerm)
      s"(($x: $xType) => $body)"
    }

    case abs: Abs =>
      sys error s"Unable to generate scala code for untyped $abs"

    case _ =>
      super.toScala(t)
  }

  // deterministic injection taking an arbitrary string
  // to a scala identifier.
  //
  // identifier ::= (letter | _) { letter | digit | _ }
  //
  def toScalaIdentifier(s: String): String =
    "_" ++ toScalaChars(s)

  // injection from arbitrary strings to strings of
  // characters allowed in scala identifiers
  def toScalaChars(s: String): String =
    s.toCharArray.flatMap { char =>
      if (char.isDigit || char.isLetter)
        Array(char)
      else
        f"_$char%X".toCharArray
    } mkString
}
