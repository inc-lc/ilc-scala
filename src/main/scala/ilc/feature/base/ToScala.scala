package ilc
package feature
package base

trait ToScala extends Syntax {
  //Contract for indentation: no indentation in the beginning
  def toScala(t: Term): String =
    s"(${toUntypedScala(t)} : ${toScala(t.getType)})"

  // subclasses should override this one without concern for types
  def toUntypedScala(t: Term): String = t match {
    case variable: Var =>
      variable.getName.toString

    case _ =>
      sys error s"Unknown term $t"
  }

  // types
  def toScala(tau: Type): String = tau match {
    case _ =>
      sys error s"Unknown type $tau"
  }

  // helper to create scala functions
  // subclasses should always call this helper to create scala
  // functions. CAUTION: supplied parameter names are binding.
  // body: no indentation in the beginning.
  private var indentDepth: Int = 2
  protected def indentMore() = { indentDepth += 2 }
  protected def indentLess() = { indentDepth -= 2 }
  protected def indentNoNl(): String = " " * indentDepth
  protected def indent(): String = "\n" + indentNoNl
  protected def openBrace() = { indentMore(); "{" }
  protected def closeBrace() = { indentLess(); s"$indent}" }

  def scalaFunction(parameterNames: String*)(body: => String): String = {
    def toParam(name: String) = name + "_param"
    def declarations = parameterNames map { name =>
      s"${indent}lazy val $name = ${toParam(name)}"
    } mkString ""
    def loop(names: Seq[String]): String =
      if (names.isEmpty)
        s"${openBrace()}${declarations}${indent}${body}${closeBrace()}"
      else
        s"${toParam(names.head)} => ${loop(names.tail)}"
    s"(${loop(parameterNames)})"
  }

  // automatic imports for generated code
  private[this]
  val features = collection.mutable.Set.empty[String]

  def addLibrary(featureName: String) {
    features += featureName
  }

  /** Imports to include in generated code. By default, the name is determined depending on this.language.
    */
  def imports: String = features map { featureName =>
    s"import ilc.feature.$featureName.Library._"
  } mkString "\n"
}
