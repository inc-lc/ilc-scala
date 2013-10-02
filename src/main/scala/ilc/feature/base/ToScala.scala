package ilc
package feature
package base

trait ToScala extends Syntax with functions.Types {
  // term constructors & polymorphic constants (TypedConst)
  def toScala(t: Term): String = t match {
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

  // automatic imports for generated code
  private[this]
  val features = collection.mutable.Set.empty[String]

  sealed trait Feature

  // a feature exports no library by default
  private[this]
  case object HasNoLibrary extends Feature

  case class HasLibrary(featureName: String) extends Feature {
    features += featureName
  }

  /** Imports to include in generated code. By default, the name is determined depending on this.language.
    */
  def imports: String = features map { featureName =>
    s"import ilc.feature.$featureName.Library._"
  } mkString "\n"

  /**
    * Feature name. Trait ilc.feature.$someName.ToScala could
    * overwrite this field to HasLibrary("$someName").
    */
  val feature: Feature = HasNoLibrary
}
