package ilc
package metaprogs

import org.scalatest._

class ClientsMemoizeSpec extends FlatSpec {
  "memoizedDerive & transform" should "produce correct results for multivariate functions" in {
    // original source file: /clients/src/main/scala/ilc/examples/MemoizationExamples.scala
    val (run0, (run1, (run2, (run3, run4)))) = examples.MemoXPlusYGenerated.program(())

    // assert that the results of the 5 runs are equal to the expected value
    assert(run0._1 == run0._2)
    assert(run1._1 == run1._2)
    assert(run2._1 == run2._2)
    assert(run3._1 == run3._2)
    assert(run4._1 == run4._2)
  }
}
