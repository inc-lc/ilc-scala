package ilc
package feature
package base

trait Typing {
  typingTrait: Syntax =>

  trait Type

  // ->: constructs term-level abstractions
  // =>: constructs function types

  case class =>: (argumentType: Type, resultType: Type) extends Type {
    override def toString: String = argumentType match {
      case _ =>: _ => s"($argumentType) => $resultType"
      case _ => s"$argumentType => $resultType"
    }
  }

  // Cf.
  // https://github.com/scala/scala/blob/v2.10.0/src/library/scala/collection/immutable/List.scala#L104
  implicit class FunctionTypeOps(resultType: Type) {
    def =>: (argumentType: Type): Type =
      new typingTrait.=>:(argumentType, resultType)
  }

  def typeOf(c: Constant): Type = throw new UntypableError(c)
  def typeOf(t: Term): Type = throw new UntypableError(t)
}

class UntypableError(x: Any)
extends Exception(s"unable to type:\n  $x")
