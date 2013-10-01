package ilc
package feature
package abelians

trait Syntax
extends products.SyntaxSugar
   with Types
{
  def abelian: TermBuilder = new PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes.head match {
        case tau =>: tau2 =>: tau3 if tau == tau2 && tau == tau3 =>
          tuple(3) ofType (
            (tau =>: tau =>: tau) =>: (tau =>: tau) =>: tau =>:
              AbelianType(tau)
          )

        case _ =>
          typeErrorNotTheSame(
            "constructing an abelian group",
            "the binary operator",
            "the argument types " +
              argumentTypes.mkString(", ")
          )
      }
  }

  def binaryOp: TermBuilder = project(1)
  def negation: TermBuilder = project(2)
  def neutral : TermBuilder = project(3)
}
