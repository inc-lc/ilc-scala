package ilc
package feature
package let

trait IsAtomic extends functions.IsAtomic {
  this: let.Syntax =>

  override def isAtomic(t: Term) =
    t match {
      case _: Let => false
      case _ => super.isAtomic(t)
    }
}
