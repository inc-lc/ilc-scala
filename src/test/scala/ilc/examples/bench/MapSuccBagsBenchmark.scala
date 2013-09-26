package ilc
package examples
package bench

/**
  * Benchmark generated derivative.
  */
object MapSuccBagsBenchmark extends NonReplacementChangeBenchmark(
  new BagIntBenchData(MapSuccBagsGenerated) {
    override def base = 10
    override def last = 50
    override def step = 10
  })
