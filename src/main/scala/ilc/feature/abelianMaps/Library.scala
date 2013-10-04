package ilc
package feature
package abelianMaps

import collection.immutable.HashMap
import abelianGroups.Library._

object Library {
  case class LiftedMapGroup[K, V](valueGroup: AbelianGroup[V])
  extends AbelianGroup[AbelianMap[K, V]]
  {
    private type T = AbelianMap[K, V]

    val binOp: T => T => T = m1 => m2 => {
      // consider using HashMap.merge instead (compare benchmarks)
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

    val inv: T => T = _.map { case (key, value) =>
      (key, valueGroup.inv(value))
    }

    val neutral: T = AbelianMap.empty

    def isEqualGroup(that: AbelianGroup[T]): Boolean = that match {
      case that @ LiftedMapGroup(_) =>
        this.valueGroup isEqualGroup that.valueGroup
    }
  }
}
