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

  type MemoizePrimMap[V] = scm.LongMap[V]
  val MemoizePrimMap = scm.LongMap

  type MemoizeObjMap[K, V] = scm.Map[K, V]
  def MemoizeObjMap[Key, Value](): MemoizeObjMap[Key, Value] = {
    import java.util.IdentityHashMap
    import scala.collection.JavaConverters._
    new IdentityHashMap[Key, Value]().asScala
  }

  //These widenings are used before hashtable lookup, so they must be injective
  //to ensure correctness.
  def widenToLong(v: Long): Long = v
  def widenToLong(v: Boolean): Long = if (v) 1 else 0

  def widenToLong(v: Double): Long =
    //I use doubleToRawLongBits over doubleToLongBits to avoid canonicalizing
    //different NaNs together.
    java.lang.Double.doubleToRawLongBits(v)

  //We could allow widening to Double, but this is more obviously accurate.
  def widenToLong(v: Float): Long =
    java.lang.Float.floatToRawIntBits(v).toLong

  def widenToLong(v: Char): Long =
    v.toLong
}
