package ilc
package feature
package functions

trait IsAtomic {
  this: functions.Syntax =>

  def isVar(t: Term) = t.isInstanceOf[Var]

  def isConst(t: Term) = isAtomic(t) && !isVar(t)

  def isAtomic(t: Term) =
    t match {
      case _: Abs => false
      case _: App => false
      case _ => true
    }
}
