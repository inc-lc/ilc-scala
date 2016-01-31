package ilc
package feature
package fixpoint

trait Derivation extends functions.Derivation with Syntax with functions.Syntax {
  override def derive(t: Term): Term = t match {
    case App(Fix(typ), body) =>
      //Since the metalanguage doesn't do type inference for us,
      //let's do it by hand.
      //Situation:
      //body: T => T
      //t = fix(body)
      //fix(body): T
      //derive(t): DT
      //derive(body) t: DT => DT
      //So fix(derive(body) t): DT, and DT must be the type parameter to fix.
      App(Fix.tapply(deltaType(typ)), App(derive(body), t))

    case _ => super.derive(t)
  }
}
