package longRunning

import org.scalameter.api._
import ilc.examples.BaseBenchmark

class BenchSuite extends BaseBenchmark {
  //include[BagUnionBenchmark]
  include[HistogramBenchmark]
  include[HistogramRecomputeBenchmark]
  //include[SumValuesBenchmark]

  override def reporters = super.reporters :+
    ChartReporter(ChartFactory.XYLine())
}
