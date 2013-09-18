package ilc
package feature
package base

/** modular derivation */
trait Derivation
extends Syntax
   with functions.Syntax // for application
{
  def deltaType(tau: Type): Type =
    throw IDontKnow(s"the Δ-type of $tau")

  /** a term that updates a value according to a change:
    *
    * update-term : ∀ {τ Γ} → Term Γ (ΔType τ → τ → τ)
    */
  // to be specialized by subclasses
  def updateTerm(tau: Type): Term =
    throw IDontKnow(s"the update-term for type $tau")

  /** a term that computes the difference of two values:
    *
    * diff-term : ∀ {τ Γ} → Term Γ (τ → τ → ΔType τ)
    */
  def diffTerm(tau: Type): Term =
    throw IDontKnow(s"the diff-term for  type $tau")

  def derive(t: Term): Term = t match {
    case x: Var =>
      DVar(x)

    case dx: DVar =>
      sys error "higher order derivatives are forbidden"

    // For all terms we don't know how to derive,
    // we produce a derivative that does recomputation.
    // This makes adding new constants easy.
    case _ =>
      Diff ! t ! t
  }

  /** @constructor creates the variable dx
    * @param original: the variable x
    *
    * The cool thing with this set up is that we can nest
    * DVars. For example, deriving `Var("x", xType) twice
    * yields
    *
    *     val ddx = DVar(Dvar(Var("x", xType)))
    *
    * such that
    *
    *     ddx.getType == deltaType(deltaType(xType))
    */
  case class DVar(original: Variable)
  extends Variable
  {
    override def getName: Name = DeltaName(original.getName)
    override def getType: Type = deltaType(original.getType)
  }

  object ChangeUpdate extends PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes take 2 match {
        case Seq(changeType, valueType)
            if changeType == deltaType(valueType) =>
          updateTerm(valueType)

        case wrongTypes =>
          typeErrorNotTheSame("specializing ChangeUpdate",
            "a delta type and a type",
            wrongTypes)
      }
  }

  object Diff extends PolymorphicTerm {
    def specialize(argumentTypes: Type*): Term =
      argumentTypes take 2 match {
        case Seq(valueType, valueType2)
            if valueType == valueType2 =>
          diffTerm(valueType)

        case wrongTypes =>
          typeErrorNotTheSame("specializing Diff",
            "two arguments of the same type",
            wrongTypes)
      }
  }
}
