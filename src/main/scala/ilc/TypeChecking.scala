package ilc

object TypeChecking extends utils.BooleanFlag {
  /**
    * Define this to false to disable some internal typechecking checks, which might be useful
    * for chaotic prototyping.
    */
  val value = true
}
