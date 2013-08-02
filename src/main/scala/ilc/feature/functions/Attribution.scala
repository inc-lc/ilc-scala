package ilc
package feature.functions

/**
 * Interface for attributing abstract syntax trees
 */

import scala.language.implicitConversions
import scala.collection.mutable

trait Attribution extends Context { self: Syntax =>

  abstract class Attribute[T](root: Term) {

    def apply(s: Subterm): T = {
      require(root == s.root)
      store(s)
    }

    // alias of this.apply for the benefit of subclasses
    protected def lookup(s: Subterm): T = apply(s)

    def update(s: Subterm, value: T) {
      require(root == s.root)
      store.update(s, value)
    }

    private[this] var store: mutable.Map[Subterm, T] =
      mutable.Map.empty
  }

  abstract class SynthesizedAttribute[T](root: Term)
  extends Attribute[T](root) {

    // ensures: attributes of all subterms are available in body.
    def synthesize(s: Subterm): T

    init(Subterm.refl(root))
    private[this] def init(s: Subterm): Unit = {
      s.eachChild(init)
      update(s, synthesize(s))
    }
  }
}
