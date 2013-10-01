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
    case sigma0 =>: sigma1 =>
      "(%s => %s)".format(toScala(sigma0), toScala(sigma1))

    case _ =>
      sys error s"Unknown type $tau"
  }

  //Subclass obligations:

  /** Imports to include in generated code. By default, the name is determined depending on this.language.
    */
  def imports: String = s"import ilc.language.$language.Libraries._"

  /**
    * Package name for the language. Trait ilc.language.$someName.ToScala should
    * define this field to "$someName".
    */
  def language: String = "bacchus"
}
