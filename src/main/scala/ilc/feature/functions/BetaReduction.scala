package ilc
package feature
package functions

trait BetaReduction extends Syntax {
  val doNormalize = false

  def betaNorm(t: Term) =
    if (doNormalize)
      betaNormalize(t, TypingContext.empty)
    else
      t

  //XXX Stub
  def betaNormalize(t: Term, typingContext: TypingContext): Term =
    t
}
