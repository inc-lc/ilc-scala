package ilc
package feature
package sums

trait Syntax extends base.Syntax with Types {
  // intro/elim forms of sum types
  // (sums of functions are forbidden)
  //
  //   either : (a → c) → (b → c) → a ⊎ b → c
  //   Inj1 : a → a ⊎ b
  //   Inj2 : b → a ⊎ b

  object Inj1
  extends ConstantWith2TypeParameters
     with SumsInjectionTrait
  {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        leftType =>: SumType(leftType, rightType)
    }

    protected[this]
    def createTerm(myArgumentType: Type, theOtherType: Type): Term =
      Inj1.tapply(myArgumentType, theOtherType)
  }

  object Inj2
  extends ConstantWith2TypeParameters
     with SumsInjectionTrait
  {
    val typeConstructor = TypeConstructor("leftType", "rightType") {
      case Seq(leftType, rightType) =>
        rightType =>: SumType(leftType, rightType)
    }

    protected[this]
    def createTerm(myArgumentType: Type, theOtherType: Type): Term =
      Inj2.tapply(theOtherType, myArgumentType)
  }

    /** to support writing the following:
      * {{{
      * Inj1(Bool) ! 1984   ofType   SumType(Nat, Bool)
      * Inj2(Nat)  ! True   ofType   SumType(Nat, Bool)
      * }}}
      */
  trait SumsInjectionTrait extends PolymorphicConstant {
    protected[this]
    def createTerm(myArgumentType: Type, theOtherType: Type): Term

    def tapply(theOtherType: Type): PolymorphicTerm =
      new PolymorphicTerm {
        def specialize(argumentTypes: Type*): Term =
          argumentTypes match {
            case Seq(myArgumentType) =>
              createTerm(myArgumentType, theOtherType)
            case _ => {
              typeErrorNotTheSame(
                s"applying $getConstantName(nonargumentType)",
                "1 type argument",
                argumentTypes.length + s" type arguments: " +
                  argumentTypes.mkString(", "))
            }
          }
      }
  }

  object Either extends ConstantWith3TypeParameters {
    val typeConstructor = TypeConstructor("a", "b", "c") {
      case Seq(a, b, c) =>
        (a =>: c) =>: (b =>: c) =>: SumType(a, b) =>: c
    }
  }
}

trait SyntaxSugar extends Syntax with functions.Syntax {
  // shorthands for pattern matching

  def case2(
    matchAgainst: TermBuilder,
    caseLeft: TermBuilder,
    caseRight: TermBuilder
  ): TermBuilder =
    Either ! caseLeft ! caseRight ! matchAgainst

  def case4(sample1: TermBuilder, sample2: TermBuilder,
            caseLL: TermBuilder, caseLR: TermBuilder,
            caseRL: TermBuilder, caseRR: TermBuilder): TermBuilder =
    case2(sample2,
      case2(sample1, caseLL, caseRL),
      case2(sample1, caseLR, caseRR))
}
