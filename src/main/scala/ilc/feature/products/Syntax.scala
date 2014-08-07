package ilc
package feature
package products

trait Syntax
extends base.Syntax
   with Types
   with functions.Types
{
  case object Pair extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        leftType =>: rightType =>: ProductType(leftType, rightType)
    }
  }

  case object Proj1 extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        ProductType(leftType, rightType) =>: leftType
    }
  }

  case object Proj2 extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        ProductType(leftType, rightType) =>: rightType
    }
  }
}

trait StdLib extends Syntax with inference.PrettySyntax {
  val pair: UntypedTerm = Pair
  val proj1: UntypedTerm = Proj1
  val proj2: UntypedTerm = Proj2
}

trait SyntaxSugar extends Syntax with functions.Syntax
{
  def tuple(n: Int): TermBuilder = {
    def loop(i: Int, vars: List[Name]): TermBuilder = {
      if (i == 0) {
        vars.reverse.map(x => x: TermBuilder).reduceRight(Pair ! _ ! _)
      } else {
        lambda(s"x${n - i + 1}") { x_i =>
          loop(i - 1, x_i :: vars)
        }
      }
    }
    if (n < 2)
      sys error s"${n}-tuples are not supported"
    else {
      loop(n, Nil)
    }
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
