package ilc
package examples

import longRunning._

class HistogramVerification extends BenchmarkVerification(
  new WordCountBenchData(HistogramGenerated) {
    override def base = 2
    override def last = 2
    override def step = 10
  })
