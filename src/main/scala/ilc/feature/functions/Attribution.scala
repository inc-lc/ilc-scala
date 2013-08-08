package ilc
package feature.functions

/**
 * Interface for attributing abstract syntax trees
 */

import scala.language.implicitConversions
import scala.collection.mutable

trait Attribution extends Context { self: Syntax =>

  trait ReadOnlyAttribute[T] {
    def lookup(s: Subterm): T
    def apply(s: Subterm): T = lookup(s)
  }

  abstract class Attribute[T](root: Term)
  extends ReadOnlyAttribute[T] {
    def update(s: Subterm, value: T): Unit
    val rootTerm = root
    val rootSubterm = Subterm.refl(root)
  }

  /**
   * The abstract class SynthesizedAttribute makes it convenient
   * to write synthesized attributes. While it is not enforced
   * that subclasses has to be synthesized attributes, a non-
   * synthesized attribute inheriting from it will almost always
   * be computed wrong.
   */
  abstract class SynthesizedAttribute[T](root: Term)
  extends Attribute[T](root) {

    def synthesize(s: Subterm, childAttr: List[T]): T

    def lookup(s: Subterm): T = lookup(s.term)
    def lookup(t: Term): T = store(t)
    def update(s: Subterm, value: T): Unit = update(s.term, value)
    def update(t: Term, value: T): Unit = store.update(t, value)

    def apply(t: Term): T = lookup(t)

    private[this] var store: mutable.Map[Term, T] =
      mutable.Map.empty

    init(rootSubterm)
    private[this] def init(s: Subterm): Unit = {
      s.eachChild(init)
      update(s,
        synthesize(s,
          s.children.map(lookup)))
    }
  }

  /**
   * The abstract class InheritedAttribute makes it convenient
   * to write inherited attributes. It is not enforced that
   * every subclass has to be an inherited attribute.
   */
  abstract class InheritedAttribute[T](root: Term)
  extends Attribute[T](root) {

    val rootAttr: T

    /**
     * Specification of an inherited attribute
     *
     * @param s
     *   subterm whose attribute is to be calculated
     * @param childNumber
     *   seniority of {@code s} w.r.t. its parent:
     *   in {@code App(p, q)}, the {@code childNumber}
     *   of {@code p} is 0 and the {@code childNumber}
     *   of {@code q} is 1.
     * @param parentAttr
     *   the attribute value of the parent of {@code s}
     */
    def inherit(parent: Subterm, parentAttr: T): List[T]

    def lookup(s: Subterm): T = store(s)
    def update(s: Subterm, value: T): Unit = store.update(s, value)

    private[this] var store: mutable.Map[Subterm, T] =
      mutable.Map.empty

    update(rootSubterm, rootAttr)
    init(Subterm.refl(root))
    
    private[this] def init(s: Subterm): Unit = {
      (s.children, inherit(s, lookup(s))).zipped.foreach(update)
    }
  }
}
