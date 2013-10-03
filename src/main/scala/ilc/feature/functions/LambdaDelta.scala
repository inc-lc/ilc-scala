package ilc
package feature
package functions

trait LambdaDelta extends base.Derivation with Syntax {
  def lambdaDelta(t: Term)
    (derivativeBody: Seq[Name] => TermBuilder): TermBuilder =
  {
    val argumentTypesOfDerivative: List[Type] =
      getArgumentTypes(t.getType) flatMap { theType =>
        List(theType, deltaType(theType))
      }
    lambda(argumentTypesOfDerivative)(derivativeBody)
  }
}
