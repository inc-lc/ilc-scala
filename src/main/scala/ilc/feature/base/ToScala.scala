package ilc
package feature
package base

trait ToScala extends Syntax with functions.Types {
  final def toScala(t: Term): String =
    s"(${toUntypedScala(t)} : ${toScala(t.getType)})"

  // subclasses should override this one without concern for types
  def toUntypedScala(t: Term): String = t match {
    case variable: Variable =>
      variable.getName.toString

    case _ =>
      sys error s"Unknown term $t"
  }

  // types
  def toScala(tau: Type): String = tau match {
    case sigma0 =>: sigma1 => {
      s"((${toScala(sigma0)}) => ${toScala(sigma1)})"
    }

    case _ =>
      sys error s"Unknown type $tau"
  }

  // helper to create scala functions
  // subclasses should always call this helper to create scala
  // functions. CAUTION: supplied parameter names are binding.
  def scalaFunction(parameterNames: String*)(body: => String): String = {
    def loop(names: Seq[String]): String =
      if (names.isEmpty)
        body
      else
        s"${names.head} => ${loop(names.tail)}"
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
