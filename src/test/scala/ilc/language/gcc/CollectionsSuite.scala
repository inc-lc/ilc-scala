package ilc
package language
package gcc

import org.scalatest.FunSuite
import org.scalatest.Matchers
import GCC._

class GCCCollectionsSuite
extends FunSuite
   with Matchers
   with Evaluation
{

  def withLibraries(body: UT) =
     typecheck { letS((collectionApi ++ math ++ points):_*)(body) }

  test("Should find integers in list") {
    withLibraries {
      list(1, 2, 3, 4, 5) contains (3, lam('x, 'y) { 'x  === 'y })
    }
  }

  test("Should find points in list") {
    withLibraries {
      list((0, 3), (1, 2), (4, 6), (0, 8), (8, 2)) contains ((4, 6), 'pointEq)
    }
  }

  test("Should create list of given size") {
    withLibraries {
      createList(4, tuple(0, 4)).head.first === 0
    }
  }

  test("Should create list of given size with multiple types one after another") {
    withLibraries {
      (createList(4, tuple(0, 4)).head.first === 0) ~: (createList(4, -1).head === -1)
    }
  }

  test("Should create maps of given size") {
    withLibraries {
      (createMap(4, 8, tuple(-1, -1)) atPos ((2, 2))) === ((-1, -1))
    }
  }

  test("Should access elements given a positional point") {
    withLibraries {
      let('a, (2, 2)) {
        createMap(4, 8, tuple(-1, -1)) atPos 'a
      }
    }

    withLibraries {
      let('a, (2, 2)) {
        (createMap(4, 8, -1) atPos 'a) ~:
        (createMap(4, 8, tuple(-1, -1)) atPos 'a)
      }
    }
  }

  test("Should update entry in map") {
    withLibraries {
      createMap(4, 8, (-1, -1)).update(Point)((2, 2), (42, 13))
    }
  }



}
