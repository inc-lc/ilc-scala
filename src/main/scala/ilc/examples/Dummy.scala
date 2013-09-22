package ilc
package examples

// ScalaMeter dummy benchmark
// to help experiment with code generation options
// and test if everything works together smoothly

trait Dummy {
  val scalaMeterDummyCode: String =
    """|package ilc
       |package examples
       |
       |import org.scalameter.api._
       |
       |object DummyGenerated extends PerformanceTest.Quickbenchmark {
       |  val sizes: Gen[Int] = Gen.range("size")(3000, 15000, 3000)
       |
       |  val ranges: Gen[Range] = for {
       |    size <- sizes
       |  } yield 0 until size
       |
       |  performance of "ilc.examples" in {
       |    performance of "Dummy" in {
       |      using(ranges) in {
       |        r => r.map(_ + 1)
       |      }
       |    }
       |  }
       |}
       |""".stripMargin
}
