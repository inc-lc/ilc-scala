package ilc
package examples
package bench

import org.scalameter.api._

class BenchSuite extends BaseBenchmark {
  include[BagUnionBenchmark]
  include[HistogramBenchmark]
  include[MapSuccBenchmark]
  include[SumValuesBenchmark]

  override def reporters = super.reporters :+
    ChartReporter(ChartFactory.XYLine())
}
