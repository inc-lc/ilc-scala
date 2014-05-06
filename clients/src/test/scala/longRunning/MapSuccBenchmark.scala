package longRunning

import ilc.examples.AbelianBagIntBenchData
import ilc.examples.BenchmarkVerification
import ilc.examples.MapSuccGenerated
import ilc.examples.NonReplacementChangeBenchmark

/**
  * Benchmark generated derivative. This is what we can achieve by
  * improving our algorithms.
  */

class MapSuccBenchmark extends NonReplacementChangeBenchmark(
  new AbelianBagIntBenchData(MapSuccGenerated) {
    override def base = 500
    override def last = 2500
    override def step = 500
  })

object MapSuccVerification extends BenchmarkVerification(
  new AbelianBagIntBenchData(MapSuccGenerated) {
    override def base = 5
    override def last = 25
    override def step = 5
  })
