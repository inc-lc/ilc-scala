package ilc
package util

trait IndentUtils {
  private var indentDepth: Int = 2

  protected def setIndentDepth(i: Int): Unit = { indentDepth = i }

  protected def indentMore() = { indentDepth += 2 }
  protected def indentLess() = { indentDepth -= 2 }
  protected def indentNoNl(): String = " " * indentDepth
  protected def indent(): String = "\n" + indentNoNl
  protected def openParen(delim: String) = { indentMore(); delim }
  protected def closeParen(delim: String) = { indentLess(); s"$indent$delim" }

  protected def openBrace() = openParen("{")
  protected def closeBrace() = closeParen("}")
}
