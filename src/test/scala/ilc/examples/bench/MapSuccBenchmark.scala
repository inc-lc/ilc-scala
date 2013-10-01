package ilc
package examples
package bench

/**
  * Benchmark hand-tuned derivative. This shows what we could achieve by
  * improving our algorithms.
  */
object MapSuccBenchmark extends ReplacementChangeBenchmark(new MapIntIntBenchData(MapSuccGenerated))
