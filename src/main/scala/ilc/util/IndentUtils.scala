package ilc
package util

trait IndentUtils {
  private var _indentDepth: Int = 2

  //This getter is only needed to allow using the setter - see http://stackoverflow.com/a/10041129/53974
  protected def indentDepth(implicit no: Nothing): Int = _indentDepth
  protected def indentDepth_=(i: Int): Unit = { _indentDepth = i }

  protected def indentMore() = { _indentDepth += 2 }
  protected def indentLess() = { _indentDepth -= 2 }
  protected def indentNoNl(): String = " " * _indentDepth
  protected def indent(): String = "\n" + indentNoNl
  protected def openParen(delim: String) = { indentMore(); delim }
  protected def closeParen(delim: String) = { indentLess(); s"$indent$delim" }

  protected def openBrace() = openParen("{")
  protected def closeBrace() = closeParen("}")
}
