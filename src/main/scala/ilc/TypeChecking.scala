package ilc

object TypeChecking extends util.BooleanFlag {
  /**
    * Define this to false to disable some internal typechecking checks, which might be useful
    * for chaotic prototyping.
    */
  val value = false
}
