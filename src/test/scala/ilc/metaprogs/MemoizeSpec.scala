package ilc
package metaprogs

import org.scalatest._
import feature._
import util.EvalScala

class MemoizeSpec extends FlatSpec with Memoize
    with memoize.Syntax
    with functions.Derivation //with let.Derivation

    with integers.ImplicitSyntaxSugar with integers.ToScala with integers.AbelianDerivation
    with functions.Syntax with let.Syntax
    with inference.PrettySyntax with inference.LetSyntaxSugar
    with inference.LetInference
    with let.Pretty with let.Traversals
    with analysis.FreeVariables
    with functions.ToScala with sums.ToScala with abelianGroups.ToScala with products.ToScala with memoize.ToScala

    with EvalScala
{
  "memoizedDerive & transform" should "complete" in {
    val memoCtx = new MemoContext()
    import memoCtx._
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
}
