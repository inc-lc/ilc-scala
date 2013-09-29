package ilc
package feature
package integers

trait Types extends base.Types {
  case object IntType extends Type { override def toString = "ℤ" }
}

trait Syntax extends base.Syntax with Types {
  case class Int(i: Int) extends Term {
    override def getType: Type = IntType
    override def toString = i.toString
  }

  case object IPlus extends Term {
    override def getType: Type =
      IntType =>: IntType =>: IntType
  }

  case object INeg extends Term {
    override def getType: Type =
      IntType =>: IntType
  }
}
