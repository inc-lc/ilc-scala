package ilc
package language
package gcc

import feature._
import scala.language.implicitConversions

trait GCCIntSyntax
extends base.Syntax
   with integers.Types
   with functions.Types
   with booleans.Types {

  case class LiteralInt(i: Int) extends Term {
    override lazy val getType: Type = IntType
  }

  class IntOp extends Term {
    override lazy val getType: Type = IntType =>: IntType =>: IntType
  }

  class IntCmpOp extends Term {
    override lazy val getType: Type = IntType =>: IntType =>: BooleanType
  }

  case object Plus extends IntOp
  case object Minus extends IntOp
  case object Mult extends IntOp
  case object Div extends IntOp

  case object Eq extends IntCmpOp
  case object Gt extends IntCmpOp
  case object Gte extends IntCmpOp

  implicit def intToTerm(n: Int): Term = LiteralInt(n)

  case object Not extends Term {
    override lazy val getType: Type = BooleanType =>: BooleanType
  }
}

trait Syntax
extends functions.Syntax
   with maybe.Syntax
   with GCCIntSyntax
   with sums.SyntaxSugar
   with products.Syntax
   with lists.Syntax
   with booleans.SyntaxSugar
   with functions.LetRecSyntax
   with products.InferenceSyntaxSugar

trait SyntaxSugar
  extends Syntax
  with inference.PrettySyntax
  with inference.SyntaxSugar
  with inference.LetRecUntypedSyntax
  with inference.LetRecInference
  with products.StdLib
  with lists.InferenceSyntaxSugar
{
  outer =>
  implicit def intToUTerm(n: Int): UntypedTerm = asUntyped(LiteralInt(n))
  def letrec(pairs: (Symbol, UntypedTerm)*)
        (bodyName: String, body: UntypedTerm): UntypedTerm = {
    ULetRec(pairs.toList map {
      case (sym, t) => (sym.name, t)
    }, bodyName, body)
  }

  type UT = UntypedTerm
  implicit def booleanToUT(b: Boolean): UT = if (b) True else False
  implicit class UTermOps[T <% UT](a: T) {
    def +(b: UT) = asUntyped(Plus)(a, b)
    def -(b: UT) = asUntyped(Minus)(a, b)
    def *(b: UT) = asUntyped(Mult)(a, b)
    def /(b: UT) = asUntyped(Div)(a, b)

    def >=(b: UT) = asUntyped(Gte)(a, b)
    def >(b: UT) = asUntyped(Gt)(a, b)
    def <=(b: UT) = b >= a
    def <(b: UT) = b > a

    def ===(b: UT) = asUntyped(Eq)(a, b)
    def =!=(b: UT) = not(a === b)

    def and(b: UT) = if_ (a) { b } else_ { false }
    def or(b: UT) = if_ (a) { true } else_ { b }
  }

  def not(a: UT) = Not(a)

  def if_(cond: UntypedTerm)(thn: UntypedTerm) = ProvideElse(cond, thn)
  case class ProvideElse(cond: UntypedTerm, thn: UntypedTerm) {
    def else_(els: UntypedTerm): UntypedTerm = asUntyped(IfThenElse)(cond, '_ ->: thn, '_ ->: els)
  }

  // Symbol + TypeAnnotation
  type NameOrTyped = Either[Symbol, TypeAnnotation]
  implicit def symbolIsLeft(s: Symbol): NameOrTyped = Left(s)
  implicit def annotationIsRight(anno: TypeAnnotation): NameOrTyped = Right(anno)

  // other syntax for functions
  def lam(firstArg: NameOrTyped, args: NameOrTyped*)(body: UntypedTerm) =
    (firstArg +: args).foldRight(body){
       case (Left(name), body) => name ->: body
       case (Right(annotation), body) => annotation ->: body
    }

  // creates a pair to be used immediately in letrec like
  //   letrec(fun('go)('n) { 'to('n + 1) })
  def fun(name: Symbol)(firstArg: NameOrTyped, args: NameOrTyped*)(body: UntypedTerm) =
    (name -> lam(firstArg, args: _*)(body))


  //For use within letS.
  //Example:
  //letS('a := 1, 'b := 2){3}
  implicit class SymBindingOps(s: Symbol) {
    def :=(t: UntypedTerm) = s -> t
  }

  implicit def consSyntax[A <% UT, B <% UT](scalaPair: (A, B)): UntypedTerm =
    pair(scalaPair._1, scalaPair._2)

  implicit class PairOps[T <% UT](t: T) {
    def first = outer.first(t)
    def second = outer.second(t)
    def at(i: Int, n: Int) = project(i, n, t)
  }
}
