package ilc
package feature
package abelianGroups

object Library extends base.Library {
  trait AbelianGroup[T] {
    def binOp: (=>T) => (=>T) => T
    def inv: (=>T) => T
    def neutral: T
    def isEqualGroup(that: AbelianGroup[T]): Boolean

    override def equals(that: Any): Boolean = that match {
      case that: AbelianGroup[T] =>
        this.isEqualGroup(that)

      case _ =>
        false
    }
  }

  object IndexedGroup {
    def curried[T]:
        (=> Int) =>
        (=> ((=>T) => (=>T) => T)) => (=>((=>T) => T)) => (=>T) =>
          AbelianGroup[T] =
      id => binOp => inv => neutral =>
        IndexedGroup(id, binOp, inv, neutral)
  }

  case class IndexedGroup[T](
    id: Int,
    binOp: (=>T) => (=>T) => T,
    inv: (=>T) => T,
    neutral: T
  )
  extends AbelianGroup[T]
  {
    def isEqualGroup(that: AbelianGroup[T]): Boolean = that match {
      case IndexedGroup(thatId, _, _, _) =>
        this.id == thatId

      case _ =>
        false
    }
  }
}
