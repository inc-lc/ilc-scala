package ilc
package feature
package abelianMaps

import abelianGroups.Library._

object Library {
  case class LiftedMapGroup[K, V](valueGroup: AbelianGroup[V])
  extends AbelianGroup[Map[K, V]]
  {
    private type T = Map[K, V]

    val binOp: T => T => T = m1 => m2 => {
      // consider using a mapBuilder instead (compare benchmarks)
      m1.foldRight[T](m2) { (keyValuePair, wipMap) => {
        val (key, value) = keyValuePair
        val newValue = wipMap.get(key).fold[V](value) {
          wipValue => valueGroup.binOp(value)(wipValue)
        }
        if (newValue == valueGroup.neutral)
          wipMap - key
        else
          wipMap.updated(key, newValue)
      }
    }}

    val inv: T => T = _.mapValues[V](valueGroup.inv)

    val neutral: T = Map.empty

    def isEqualGroup(that: AbelianGroup[T]): Boolean = that match {
      case that @ LiftedMapGroup(_) =>
        this.valueGroup isEqualGroup that.valueGroup
    }
  }
}
