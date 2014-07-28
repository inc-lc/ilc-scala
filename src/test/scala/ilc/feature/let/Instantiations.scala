package ilc
package feature
package let

trait ShowTerms {
  this: Pretty =>

  def show(t: Term) =
    "\n" + pretty(t)
}

trait Instantiations {
  def buildBacchusWithLetSystem(doCSE_ : Boolean, copyPropagation_ : Boolean, partialApplicationsAreSpecial_ : Boolean) =
    new language.Bacchus with let.ANormalFormAdapter with integers.ImplicitSyntaxSugar
      with integers.Evaluation with let.Evaluation with let.Pretty
      with inference.LetInference
      with BetaReduction with inference.LetSyntaxSugar with inference.InferenceTestHelper with ShowTerms {
        outer =>
        val aNormalizer = new ANormalFormStateful {
          val mySyntax: outer.type = outer
          override val doCSE = doCSE_
          override val copyPropagation = copyPropagation_
          override val partialApplicationsAreSpecial = partialApplicationsAreSpecial_
        }
    }

  def buildBaseBacchus() =
    new language.Bacchus with integers.ImplicitSyntaxSugar with inference.LetInference
      with BetaReduction with Pretty
      with products.StdLib
      with inference.LetSyntaxSugar
      with ShowTerms

  def buildCacher[T <: Syntax with IsAtomic with products.SyntaxSugar with unit.Syntax with Traversals](bacchus: T) =
    new AddCaches2 {
      val mySyntax: bacchus.type = bacchus
    }
}

object WorksheetHelpers extends Instantiations {
  val bacchus = buildBaseBacchus()
  val cacher = buildCacher(bacchus)
}
