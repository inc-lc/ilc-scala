package ilc
package feature
package let

/**
 * Derivation for Let.
 * This is untested. That's also because Lets don't arise before
 * derivation in the <em>current</em> language pipeline.
 */
trait Derivation extends base.Derivation {
  this: Syntax =>

  override def derive(t: Term): Term = t match {
    case Let(x, term, body) =>
      Let(x, term,
        Let(DVar(x), derive(term), derive(body)))
    case _ =>
      super.derive(t)
  }
}
