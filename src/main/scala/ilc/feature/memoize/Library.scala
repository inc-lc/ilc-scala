package ilc
package feature
package memoize

object Library {
  //This should be in some standard library — it's a thread-unsafe version of scala.concurrent.SyncVar
  class Cell[T](initialContent: T) {
    var value: T = initialContent
    def apply() = value
    def update(newValue: T) {
      value = newValue
    }
  }

  class OptCell[T] {
    val box: Cell[Option[T]] = new Cell(None)
    def getOrElseUpdate(newValue: => T): T = {
      val curr = box()
      curr match {
        case Some(v) => v
        case None =>
          val ret = newValue
          box() = Some(ret)
          ret
      }
    }
  }

  object OptCell {
    def apply[T](initialContent: T): OptCell[T] = {
      val ret = new OptCell[T]
      ret.box() = Some(initialContent)
      ret
    }
    def apply[T]() = new OptCell[T]
  }

  //Reexport relevant maps.
  import scala.collection.{mutable => scm}

  type HashMap[K, V] = scm.HashMap[K, V]
  val HashMap = scm.HashMap
  type LongMap[V] = scm.LongMap[V]
  val LongMap = scm.LongMap
}
