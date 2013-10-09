package ilc
package feature
package bags

import collection.immutable.HashMap
import collection.mutable.Stack

object Library extends base.Library {
  import abelianGroups.Library._

  type Bag[T] = HashMap[T, Int]

  object Bag {
    def apply[T](elements: T*) =
      elements.foldRight(bagEmpty[T]) { (element, bag) =>
        bag.get(element).fold(bag.updated(element, 1)) { i =>
          bag.updated(element, i + 1)
        }
      }
  }

  def bagEmpty[T]: Bag[T] = HashMap.empty[T, Int]

  def bagSingleton[T]: (=>T) => Bag[T] = t => HashMap(t -> 1)

  //XXX: Could be made much faster by using builders (avoiding immutable
  //copies), but this shouldn't be necessary.
  def bagUnion[T]:
      (=>Bag[T]) => (=>Bag[T]) => Bag[T] = b1Param => b2Param => {
    lazy val b1 = b1Param
    lazy val b2 = b2Param
    val toDelete = Stack.empty[T]
    (b1 merged b2) {
      case ((el1, count1), (el2, count2)) =>
        assert(el1 == el2)
        val sum = count1 + count2
        if (sum == 0) toDelete push el1
        el1 -> (count1 + count2)
    } -- toDelete
  }

  def bagFoldGroup[G, T]:
      (=>AbelianGroup[G]) => (=>(=>T) => G) => (=>Bag[T]) => G =
    abelianparam => fParam => bagParam =>
  {
    lazy val abelian = abelianparam
    lazy val f = fParam
    lazy val bag = bagParam
    val (op, inv, neutral) = (abelian.binOp, abelian.inv, abelian.neutral)
    (for {
      (t, count) <- bag
      g = f(t)
      (h, posCount) = if (count >= 0) (g, count) else (inv(g), -count)
      e <- Seq.tabulate(posCount)(_ => h)
    } yield e).foldRight[G](neutral){ case (elem1, elem2) =>
        op(elem1)(elem2)
    }
  }

  def bagNegate[T]:(=>Bag[T]) => Bag[T] = _ map {case (k, v) => (k, -v)}

  case class FreeAbelianGroup[T]() extends AbelianGroup[Bag[T]] {
    val binOp   = bagUnion[T]
    val inv     = bagNegate[T]
    val neutral = bagEmpty[T]
    def isEqualGroup(that: AbelianGroup[Bag[T]]): Boolean = that match {
      case FreeAbelianGroup() =>
        true

      case _ =>
        false
    }
  }
}
