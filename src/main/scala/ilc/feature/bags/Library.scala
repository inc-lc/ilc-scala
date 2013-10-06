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

  def bagSingleton[T](t: T): Bag[T] = HashMap(t -> 1)

  //XXX: Could be made much faster by using builders (avoiding immutable
  //copies), but this shouldn't be necessary.
  def bagUnion[T](b1: Bag[T])(b2: Bag[T]): Bag[T] = {
    val toDelete = Stack.empty[T]
    (b1 merged b2) {
      case ((el1, count1), (el2, count2)) =>
        assert(el1 == el2)
        val sum = count1 + count2
        if (sum == 0) toDelete push el1
        el1 -> (count1 + count2)
    } -- toDelete
  }

  def bagFoldGroup[G, T]
    (abelian: AbelianGroup[G])(f: T => G)(bag: Bag[T]): G =
  {
    val (op, inv, neutral) = (abelian.binOp, abelian.inv, abelian.neutral)
    (for {
      (t, count) <- bag
      g = f(t)
      (h, posCount) = if (count >= 0) (g, count) else (inv(g), -count)
      e <- Seq.tabulate(posCount)(_ => h)
    } yield e).foldRight[G](neutral)(Function.uncurried(op))
  }

  def bagNegate[T](b: Bag[T]): Bag[T] = b map {case (k, v) => (k, -v)}

  case class FreeAbelianGroup[T]() extends AbelianGroup[Bag[T]] {
    val binOp   = bagUnion[T] _
    val inv     = bagNegate[T] _
    val neutral = bagEmpty[T]
    def isEqualGroup(that: AbelianGroup[Bag[T]]): Boolean = that match {
      case FreeAbelianGroup() =>
        true

      case _ =>
        false
    }
  }
}
