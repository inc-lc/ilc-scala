package ilc
package feature.functions

trait UniqueNames extends Syntax with FV {

  object uniqueName extends ((Set[String], String) => String) {

    def apply(toAvoid: Set[String], default: String): String =
      if (! toAvoid.contains(default))
        default
      else {
        var i = 0
        def name = default ++ toSubscript(i)
        do i += 1
        while(toAvoid.contains(name))
        name
      }


    def toSubscript(i: Int): String = toSubscript(i.toString)

    def toSubscript(s: String): String = {
      s.map({ (char: Char) =>
        if (char.isDigit) subscript(char - '0') else char
      }).mkString
    }

    val subscript = "₀₁₂₃₄₅₆₇₈₉".toCharArray
  }

  def uniqueName(t: Term, default: String): String =
    uniqueName(FV(t), default)

  def uniqueNames(toAvoid: Set[String],
                  defaults: String*): List[String] =
    if (defaults.isEmpty)
      Nil
    else {
      val firstName = uniqueName(toAvoid, defaults.head)
      val otherNames = uniqueNames(toAvoid + firstName, defaults.tail: _*)
      firstName :: otherNames
    }

  def uniqueNames(t: Term, defaults: String*): List[String] =
    uniqueNames(FV(t), defaults: _*)

  def uniqueVars(toAvoid: Set[String], defaults: String*): List[Var] =
    uniqueNames(toAvoid, defaults: _*) map Var

  def uniqueVars(t: Term, defaults: String*): List[Var] =
    uniqueNames(t, defaults: _*) map Var
}
