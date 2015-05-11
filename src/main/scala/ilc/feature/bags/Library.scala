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
        bag.updated(element, 1 + bag.get(element).fold(0) { i => i })
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
    val collisionHandler: (((T, Int), (T, Int))) => (T, Int) = {
      case ((el1, count1), (el2, count2)) =>
        assert(el1 == el2)
        val sum = count1 + count2
        if (sum == 0) toDelete push el1
        el1 -> sum
    }
    // to work around a scala bug:
    // sometimes `merged` calls the collision handler with null arguments!
    (b1 merged b2) {
      case (null, null) =>
        sys error "No clue when merging bags, giving up."

      case ((el1, count1), null) =>
        collisionHandler(((el1, count1), (el1, b2(el1))))

      case (null, (el2, count2)) =>
        collisionHandler(((el2, b1(el2)), (el2, count2)))

      case otherwise =>
        collisionHandler(otherwise)
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
