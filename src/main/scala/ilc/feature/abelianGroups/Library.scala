package ilc
package feature
package abelianGroups

object Library extends base.Library {
  trait AbelianGroup[T] {
    def binOp: T => T => T
    def inv: T => T
    def neutral: T
    def isEqualGroup(that: AbelianGroup[T]): Boolean
  }

  object GenerativeGroup {
    def curried[T]:
        Int => (T => T => T) => (T => T) => T => AbelianGroup[T] =
      id => binOp => inv => neutral =>
        GenerativeGroup(id, binOp, inv, neutral)
  }

  case class GenerativeGroup[T](
    id: Int,
    binOp: T => T => T,
    inv: T => T,
    neutral: T
  )
  extends AbelianGroup[T]
  {
    def isEqualGroup(that: AbelianGroup[T]): Boolean = that match {
      case GenerativeGroup(thatId, _, _, _) =>
        this.id == thatId

      case _ =>
        false
    }
  }
}
