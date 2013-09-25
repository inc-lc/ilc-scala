package ilc
package examples
package bench

/**
  * Benchmark generated derivative.
  */
object MapSuccBagsBenchmark extends ExampleToBenchmark(
  new BagIntBenchData(MapSuccBagsGenerated) {
    override def base = 10
    override def last = 50
    override def step = 10
  })
