package ilc
package examples
package bench

/**
  * Benchmark generated derivative.
  */
object MapSuccBagsBenchmark extends NonReplacementChangeBenchmark(
  new BagIntBenchData(MapSuccBagsGenerated) {
    override def base = 50
    override def last = 250
    override def step = 50
  })
