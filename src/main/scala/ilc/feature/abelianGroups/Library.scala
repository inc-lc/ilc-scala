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
}
