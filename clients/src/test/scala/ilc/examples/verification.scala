package ilc
package examples

import longRunning._

object BagUnionVerification extends BenchmarkVerification(
  new BagPairBenchData(BagUnionGenerated) {
    override def base = 5
    override def last = 15
    override def step = 5
  })

object MapSuccVerification extends BenchmarkVerification(
  new AbelianBagIntBenchData(MapSuccGenerated) {
    override def base = 5
    override def last = 25
    override def step = 5
  })

object SumValuesVerification extends BenchmarkVerification(
  new AdditiveMapBenchData(SumValuesGenerated) {
    override def base = 5
    override def last = 25
    override def step = 5
  })
