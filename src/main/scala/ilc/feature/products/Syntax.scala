package ilc
package feature
package products

trait Syntax
extends base.Syntax
   with Types
   with functions.Types
{
  object Pair extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        leftType =>: rightType =>: ProductType(leftType, rightType)
    }
  }

  object Proj1 extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        ProductType(leftType, rightType) =>: leftType
    }
  }

  object Proj2 extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        ProductType(leftType, rightType) =>: rightType
    }
  }
}

trait SyntaxSugar extends Syntax with functions.Syntax
{
  /** As in Agda, tuples are nested toward the right.
    * If tuples are nested toward the left,
    * then proj(i) is ill-defined.
    */

  def tupleType(elementTypes: Type*): Type =
    if (elementTypes.size == 1)
      elementTypes.head
    else
      ProductType(elementTypes.head, tupleType(elementTypes.tail: _*))

  def tuple(n: Int): TermBuilder = {
    def loop(i: Int, prefix: TermBuilder): TermBuilder = {
      if (i == 2)
        lambda(s"x${n - 1}", s"x$n") { case Seq(penult, last) =>
          prefix ! (Pair ! penult ! last)
        }
      else
        lambda(s"x${n - i + 1}") { x_j =>
          loop(i - 1, prefix composeWithBuilder (Pair ! x_j))
        }
    }
    if (n == 2)
      Pair
    else if (n > 2)
      lambda("x1") { x1 => loop(n - 1, Pair ! x1) }
    else
      sys error s"${n}-tuples are not supported"
  }

  def project(i: Int): TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case tupleType@ProductType(leftType, rightType) =>
          if (i == 1)
            Proj1(leftType, rightType)
          else if (i == 2)
            rightType match {
              case ProductType(carType, cdrType) =>
                Proj1 composeWith Proj2(leftType, rightType)

              case _ =>
                Proj2(leftType, rightType)
            }
          else if (i > 2)
            project(i - 1) composeWith Proj2(leftType, rightType)
          else
            sys error s"cannot project onto the ${i}th element"
      }
  }
}
