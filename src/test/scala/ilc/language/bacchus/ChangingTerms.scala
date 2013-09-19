package ilc
package language
package bacchus

trait ChangingTerms extends feature.base.Syntax {
  // shorthand for changing terms
  case class ChangingTerms(oldTerm: Term, newTerm: Term)
  implicit class ChangingTermsInfixConstructor[T <% Term](oldTerm: T) {
    def ↦ (newTerm: Term) = ChangingTerms(oldTerm, newTerm)
  }
}
