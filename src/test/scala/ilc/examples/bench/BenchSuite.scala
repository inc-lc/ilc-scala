package ilc
package examples
package bench

class BenchSuite extends BaseBenchmark {
  include[BagUnionBenchmark]
  include[HistogramBenchmark]
  include[MapSuccBenchmark]
  include[SumValuesBenchmark]
}
