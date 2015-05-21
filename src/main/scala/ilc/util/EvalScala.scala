package ilc
package util

/**
 * "eval" for scala
 * CAUTION: code is evaluated in empty context.
 * Local names are _not_ visible.
 */

import ilc.util.process.FunProcess

// Using scala.reflect internal for now.
// With the soon-to-be-released scala-2.11,
// we can switch to JSR-223:
// javax.script.ScriptEngineManager.getEngineByName("scala")
//
// Ref.
// http://stackoverflow.com/a/12123609

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

trait EvalScala
{
  /**
    * Evaluate scala code and load the class.
    * Warning: because of a ToolBox bug (https://issues.scala-lang.org/browse/SI-6393),
    * using imports is only partly supported.
    */
  def evalScala(scalaCode: String): Any = {
    toolBox.eval(toolBox.parse(scalaCode))
  }

  val toolBox = getToolbox(this)

  def getToolbox(scopeObject: Any) =
    universe.runtimeMirror(scopeObject.getClass.getClassLoader).mkToolBox()
}

trait EvalGenerated extends feature.base.ToScala with EvalScala {
  def evalGenerated(t: Term): Any =
    evalScala(s"""|{
                  |  $imports
                  |  ${toScala(t)}
                  |}""".stripMargin)
}
