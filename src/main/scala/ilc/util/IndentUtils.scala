package ilc
package util

trait IndentUtils {
  protected val indentDiff: Int = 2
  protected def initialIndentDepth: Int = 0
  private var indentDepth: Int = initialIndentDepth

  protected def setIndentDepth(i: Int): Unit = { indentDepth = i }

  protected def indentMore(delta: Int = indentDiff): String = { indentDepth += delta; "" }
  protected def indentLess(delta: Int = indentDiff): String = { indentDepth -= delta; "" }
  protected def deeper(arg: => String, indentDelta: Int = indentDiff): String = {
    indentMore(indentDelta)
    val ret = arg
    indentLess(indentDelta)
    ret
  }

  protected def indentNoNl(): String = " " * indentDepth
  protected def indent(): String = "\n" + indentNoNl
  protected def openParen(delim: String) = { indentMore(); delim }
  protected def closeParen(delim: String) = { indentLess(); s"$indent$delim" }

  protected def openBrace() = openParen("{")
  protected def closeBrace() = closeParen("}")
}
