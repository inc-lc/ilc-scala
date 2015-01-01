package ilc
package metaprogs

import org.scalatest._
import feature._

class AlphaEquivSpec extends FlatSpec with Matchers with AlphaEquiv with inference.SyntaxSugar {
  def shouldBeAlphaEquiv(t1: Term, t2: Term, ignoreTypes: Boolean = false) =
    assert(alphaEquiv(t1, t2, ignoreTypes))

  def shouldBeAlphaDiff(t1: Term, t2: Term, ignoreTypes: Boolean = false) =
    assert(!alphaEquiv(t1, t2, ignoreTypes))

  "alpha-equivalence" should "identify equal terms" in {
    val t = asTerm('x ->: 'x)
    shouldBeAlphaEquiv(t, t)
  }

  it should "identify identical terms with different types (when ignoring types)" in {
    shouldBeAlphaEquiv('x ->: 'x, 'x ->: 'x, true)
  }

  it should "distinguish identical terms with different type variables" in {
    shouldBeAlphaDiff('x ->: 'x, 'x ->: 'x)
  }

  it should "identify alpha-equivalent terms" in {
    val tv = _freshTypeVariable(None)
    shouldBeAlphaEquiv('x % tv ->: 'x, 'y % tv ->: 'y)
  }

  it should "identify alpha-equivalent terms with different types (when ignoring types)" in {
    shouldBeAlphaEquiv('x ->: 'x, 'y ->: 'y, true)
  }

  it should "distinguish alpha-equivalent terms with different type variables" in {
    shouldBeAlphaDiff('x ->: 'x, 'y ->: 'y)
  }

  it should "distinguish terms which differ only in names" in {
    shouldBeAlphaDiff('x ->: 'y ->: 'x, 'x ->: 'y ->: 'y, true)
  }

  it should "distinguish variables with the same name in different contexts" in {
    shouldBeAlphaDiff('x ->: 'y ->: 'x, 'y ->: 'x ->: 'x, true)
  }

  it should "identify identical terms with different names" in {
    shouldBeAlphaEquiv('x ->: 'y ->: 'x, 'y ->: 'x ->: 'y, true)
  }
}
