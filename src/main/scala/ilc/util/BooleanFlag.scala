package ilc
package util

//Using a trait makes life harder for the inliner and gives annoying warnings.
abstract class BooleanFlag {
  def value: Boolean

  @inline def choose[T](ifTrue: => T, ifFalse: => T): T =
    if (value) ifTrue else ifFalse
}
