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

trait InferenceSyntaxSugar extends Syntax with functions.Syntax with inference.SyntaxSugar
{
  def tuple(firstArg: UntypedTerm, args: UntypedTerm*) =
    (firstArg +: args).reduceRight(Pair(_, _))

  //i is 0-based.
  def project(i: Int, n: Int, t: UntypedTerm): UntypedTerm =
    if (i < 0)
      sys error s"${i}-th tuple projections are not supported (tuple indexes start from 0)"
    else if (i >= n)
      sys error s"Can't project ${i}-th element out of ${n}-ary tuple, valid indexes are from 0 to ${n - 1}"
    else if (n < 2)
      sys error s"${n}-tuples are not supported"
    else {
      if (i == 0)
        Proj1(t)
      else if (i == 1 && n == 2)
        Proj2(t)
      else
        project(i - 1, n - 1, Proj2(t))
    }
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
            Proj1.tapply(leftType, rightType)
          else if (i == 2)
            rightType match {
              case ProductType(carType, cdrType) =>
                Proj1 composeWith Proj2.tapply(leftType, rightType)

              case _ =>
                Proj2.tapply(leftType, rightType)
            }
          else if (i > 2)
            project(i - 1) composeWith Proj2.tapply(leftType, rightType)
          else
            sys error s"cannot project onto the ${i}th element"
      }
  }
}
