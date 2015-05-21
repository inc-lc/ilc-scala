package ilc
package metaprogs

import org.scalatest._
import feature._
import util.EvalScala

class MemoizeSpec extends FlatSpec with Memoize
    with MemoizeSyntax
    with functions.Derivation //with let.Derivation

    with integers.ImplicitSyntaxSugar with integers.ToScala with integers.AbelianDerivation
    with functions.Syntax with let.Syntax
    with inference.PrettySyntax with inference.LetSyntaxSugar
    with inference.LetInference
    with let.Pretty
    with analysis.FreeVariables
    with functions.ToScala with sums.ToScala with abelianGroups.ToScala with products.ToScala with MemoizeToScala

    with EvalScala
{
  "stuff" should "work" in {
    val t = asTerm(('x % IntType) ->: ('y % IntType) ->: 'x)
    val tTransf = transform(t)
    println(tTransf)
    println(pretty(tTransf))
    println(toScala(tTransf))
    val tDeriv = memoizedDerive(t)
    println(tDeriv)
    println(pretty(tDeriv))
    println(toScala(tDeriv))

    {
      val t2 = asTerm(('x % IntType) ->: ('y % IntType) ->: ('z % IntType) ->: (PlusInt(PlusInt('x, 'y), 'z)))
      val t2Transf = transform(t2)
      println(t2Transf)
      println(pretty(t2Transf))
      println(toScala(t2Transf))
      val t2Deriv = memoizedDerive(t2)
      println(t2Deriv)
      println(pretty(t2Deriv))
      println(toScala(t2Deriv))
    }
    println(cacheMap)
  }

  def addImports(code: String): String =
    s"""|{ // add imports here
        |  import ilc.feature.abelianGroups.Library._
        |  import ilc.metaprogs._ // for OptCell etc. living in this package temporarily
        |  import scala.collection.{mutable => scm}
        |  $code
        |}""".stripMargin

  def compile[T](t: Term): T = evalScala(addImports(toScala(t))).asInstanceOf[T]

  "memoizedDerive" should "produce code that compiles to well-typed Scala" in {
    val t = asTerm(('x % IntType) ->: ('y % IntType) ->: (PlusInt('x, 'y)))
    compile[Any](memoizedDerive(t))
  }

  ignore should "produce correct results for multivariate functions" in {
    import abelianGroups.Library._

    type DInt = ((=> Int) => Either[(AbelianGroup[Int], Int), Int])
    val t  = asTerm(('x % IntType) ->: ('y % IntType) ->: (PlusInt('x, 'y)))
    val dt = memoizedDerive(t)
    val f  = compile[(=> Int) => (=> Int) => Int](t)
    val df = compile[(=> Int) => DInt => (=> Int) => DInt => Int](dt)
    assert(f(1)(2) == 3)
    // TODO: assert derivative produce correct change when given changing x and unchanging y
  }
}
