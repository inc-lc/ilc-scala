package ilc
package language

import feature._

/**
  * A variant of Bacchus with support for Let.
  */
class LetLanguage extends
    language.Bacchus with integers.Syntax with integers.ImplicitSyntaxSugar with inference.LetInference
      with let.BetaReduction with let.Pretty
      with products.StdLib
      with inference.LetSyntaxSugar
      with let.ShowTerms
      with letLanguage.Evaluation
