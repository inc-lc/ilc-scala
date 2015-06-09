package ilc
package examples
package handwritten

import org.scalameter.api._
import scala.collection.{mutable, immutable}

class BagBench extends MyBenchmarkingSetup {
  val sizes = Gen.range("n")(250, 1000, 250)

  performance of "nestedLoop1 immutable Map" in {
    using(sizes) config myBenchConfig in { size =>
      (1 to size).flatMap { i =>
        1 to size map { j =>
          (i * size + j, 1)
        }
      }(collection.breakOut): immutable.Map[Int, Int]
    }
  }

  performance of "nestedLoop1 immutable Seq (?)" in {
    using(sizes) config myBenchConfig in { size =>
      1 to size flatMap { i =>
        1 to size map { j =>
          i * size + j
        }
      }
    }
  }

  performance of "nestedLoop1 immutable Set" in {
    using(sizes) config myBenchConfig in { size =>
      (1 to size).flatMap { i =>
        1 to size map { j =>
          i * size + j
        }
      }(collection.breakOut): immutable.Set[Int]
    }
  }

  performance of "nestedLoop1 mutable Map" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = mutable.Map[Int, Int]()
      for {
        i <- 1 to size
        j <- 1 to size
      } {
        coll += (i * size + j -> 1)
      }
    }
  }

  performance of "nestedLoop1 mutable Set" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = mutable.Set[Int]()
      for {
        i <- 1 to size
        j <- 1 to size
      } {
        coll += i * size + j
      }
    }
  }

  performance of "nestedLoop1 mutable ArrayBuffer" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = mutable.ArrayBuffer[Int]()
      for {
        i <- 1 to size
        j <- 1 to size
      } {
        coll += i * size + j
      }
    }
  }

  performance of "nestedLoop1 mutable Java Array" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = new java.util.ArrayList[Int]()
      for {
        i <- 1 to size
        j <- 1 to size
      } {
        coll add (i * size + j)
      }
    }
  }

  performance of "nestedLoop1 mutable Java Set" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = new java.util.HashSet[Int]()
      for {
        i <- 1 to size
        j <- 1 to size
      } {
        coll add (i * size + j)
      }
    }
  }

  performance of "nestedLoop1 mutable Java Array w/ while loops" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = new java.util.ArrayList[Int]()
      var i = 1
      while (i <= size) {
        var j = 1
        while (j <= size) {
          coll add (i * size + j)
          j += 1
        }
        i += 1
      }
    }
  }

  performance of "nestedLoop1 mutable Java Set w/ while loops" in {
    using(sizes) config myBenchConfig in { size =>
      val coll = new java.util.HashSet[Int]()
      var i = 1
      while (i <= size) {
        var j = 1
        while (j <= size) {
          coll add (i * size + j)
          j += 1
        }
        i += 1
      }
    }
  }
}
