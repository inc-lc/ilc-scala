package ilc
package util

trait IndentUtils {
  private var indentDepth: Int = 2
  protected def indentMore() = { indentDepth += 2 }
  protected def indentLess() = { indentDepth -= 2 }
  protected def indentNoNl(): String = " " * indentDepth
  protected def indent(): String = "\n" + indentNoNl
  protected def openBrace() = { indentMore(); "{" }
  protected def closeBrace() = { indentLess(); s"$indent}" }
}
