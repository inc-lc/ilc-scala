package ilc
package feature
package functions

trait IsAtomic {
  this: functions.Syntax =>

  def isAtomic(t: Term) =
    t match {
      case _: Abs => false
      case _: App => false
      case _ => true
    }
}
