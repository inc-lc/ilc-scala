package bench

import ilc.examples.MapIntIntBenchData
import ilc.examples.MapSuccBaseGenerated
import ilc.examples.ReplacementChangeBenchmark

/**
  * Benchmark generated derivative.
  */
object MapSuccBaseBenchmark extends ReplacementChangeBenchmark(new MapIntIntBenchData(MapSuccBaseGenerated))
