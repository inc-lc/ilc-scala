package ilc
package feature
package base

trait Typing {
  typingTrait: Syntax =>

  trait Type

  // ->: constructs term-level abstractions

  /** Function type constructor as a case class.
    */
  case class =>:(argumentType: Type, resultType: Type) extends Type {
    override def toString: String = argumentType match {
      case _ =>: _ => s"($argumentType) => $resultType"
      case _ => s"$argumentType => $resultType"
    }
  }

  /** Function-type constructor as a convenient infix method, right-associative as usual.
    */
  // The argument order is correct because in Scala right-associative operators
  // are invoked on their right-hand side (see SLS 6.2.3).
  implicit class FunctionTypeOps(resultType: Type) {
    def =>:(argumentType: Type): Type =
      new typingTrait.=>:(argumentType, resultType)
  }

  //Subclass obligation:
  type Context
  def emptyContext: Context

  //Subclass obligations {{{
  def typeOf(c: Constant): Type = throw new UntypableError(c)

  /** Compute type of a term in a given typing context.
    */
  def typeOf(t: Term, initialContext: Context): Type = throw new UntypableError(t)
  //}}}

  /** Compute type of a closed term.
    */
  final def typeOf(t: Term): Type =
    typeOf(t, emptyContext)
}

class UntypableError(x: Any)
extends Exception(s"unable to type:\n  $x")
