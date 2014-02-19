package bench

import ilc.examples.BagIntBenchData
import ilc.examples.MapSuccBagsGenerated
import ilc.examples.NonReplacementChangeBenchmark

/**
  * Benchmark generated derivative.
  */
object MapSuccBagsBenchmark extends NonReplacementChangeBenchmark(
  new BagIntBenchData(MapSuccBagsGenerated) {
    override def base = 50
    override def last = 250
    override def step = 50
  })
