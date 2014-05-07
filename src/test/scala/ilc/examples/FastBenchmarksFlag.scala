package ilc
package examples

object FastBenchmarksFlag extends utils.BooleanFlag {
  /**
    * Define this to true to speed up benchmarking by reducing the maximum
    * dataset size, and the memory allocated by the sub-JVMs set via the -Xmx
    * option.
    */
  val value = true
}
