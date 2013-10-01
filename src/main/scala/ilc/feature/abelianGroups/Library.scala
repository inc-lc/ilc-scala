package ilc
package feature
package abelianGroups

trait Library {
  trait AbelianGroup[T] {
    def binOp: T => T => T
    def inv: T => T
    def neutral: T
    def isEqualGroup(that: AbelianGroup[T]): Boolean
  }

  object GenerativeGroup {
    private[this] var counter = 0

    private def nextId(): Int = {
      counter += 1
      counter
    }

    def curried[T]: (T => T => T) => (T => T) => T => AbelianGroup[T] =
      binOp => inv => neutral => GenerativeGroup(binOp, inv, neutral)
  }

  case class GenerativeGroup[T](
    binOp: T => T => T,
    inv: T => T,
    neutral: T
  )
  extends AbelianGroup[T]
  {
    val id = GenerativeGroup.nextId()

    def isEqualGroup(that: AbelianGroup[T]): Boolean = that match {
      case that @ GenerativeGroup(_, _, _) =>
        this.id == that.id

      case _ =>
        false
    }
  }

  // TODO: Move me to abelianMaps
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
