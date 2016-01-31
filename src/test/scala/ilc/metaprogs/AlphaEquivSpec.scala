package ilc
package metaprogs

import org.scalatest._
import feature._

class AlphaEquivSpec extends FlatSpec
    with integers.ImplicitSyntaxSugar
    with let.Syntax
    with inference.PrettySyntax with inference.LetSyntaxSugar
    with inference.LetInference

    with feature.maps.Syntax
    with feature.bags.StdLib
    with feature.abelianMaps.Syntax
    with base.Pretty

    with AlphaEquivLet {
  def shouldBeAlphaEquiv(t1: Term, t2: Term, ignoreTypes: Boolean = false) =
    assert(alphaEquiv(t1, t2, ignoreTypes))

  def shouldBeAlphaDiff(t1: Term, t2: Term, ignoreTypes: Boolean = false) =
    assert(!alphaEquiv(t1, t2, ignoreTypes))

  val idUt = 'x ->: 'x
  val ut1 = letS('x := ifThenElse_(True, asTerm(1), asTerm(2)))('x)

  val ut2 = 'x ->: 'y ->: letS('z := ifThenElse_(True, 'x, 'y))('z)
  val ut3 = 'f ->: 'g ->: FoldGroup(LiftGroup(FreeAbelianGroup),
    'e ->: SingletonMap('f('e), Singleton('g('e))))

  val ut4 = letS('x := ifThenElse_(True, asTerm(1), asTerm(2)))('x)

  "alpha-equivalence" should "identify identical terms (even when ignoring types)" in {
    //asTerm triggers type inference once:
    val t = asTerm(idUt)
    shouldBeAlphaEquiv(t, t)
    shouldBeAlphaEquiv(t, t, true)
    val t1 = asTerm(ut1)
    shouldBeAlphaEquiv(t1, t1)
    shouldBeAlphaEquiv(t1, t1, true)

    //Let's use polymorphic constants
    val t2 = asTerm(ut2)
    shouldBeAlphaEquiv(t2, t2)
    shouldBeAlphaEquiv(t2, t2, true)

    val t3 = asTerm(ut3)

    shouldBeAlphaEquiv(t3, t3)
    shouldBeAlphaEquiv(t3, t3, true)

    //Fully monomorphic type, no call to asTerm is needed.
    shouldBeAlphaEquiv(ut4, ut4)
    shouldBeAlphaEquiv(ut4, ut4, true)
  }

  it should "identify identical terms with different types (when ignoring types)" in {
    // The implicit conversion doing inference is effectful. The resulting terms will have
    // what are morally alpha-equivalent types (with different fresh variables),
    // but since they don't contain quantifiers we can't detect that (unless
    // we implicitly universally quantify type variables -- doing that
    // implicitly is a bit of a hack, and not cleanly compatible with
    // first-class polymorphism, even though Hindley-Milner languages do
    // that and suffer the consequences.
    shouldBeAlphaEquiv(idUt, idUt, true)
    shouldBeAlphaEquiv(ut2, ut2, true)
    //A different test
    shouldBeAlphaEquiv(ut4, ut4, true)
  }

  it should "distinguish identical terms with different type variables" in {
    shouldBeAlphaDiff(idUt, idUt)
    shouldBeAlphaDiff(ut2, ut2)
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
