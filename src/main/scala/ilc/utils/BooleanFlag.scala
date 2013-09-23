package ilc
package utils

trait BooleanFlag {
  def value: Boolean

  @inline def choose[T](ifTrue: => T, ifFalse: => T): T =
    if (value) ifTrue else ifFalse
}
