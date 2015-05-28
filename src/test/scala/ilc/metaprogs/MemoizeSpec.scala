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
    with let.Pretty
    with analysis.FreeVariables
    with functions.ToScala with sums.ToScala with abelianGroups.ToScala with products.ToScala with memoize.ToScala

    with EvalScala
{
  "memoizedDerive & transform" should "complete" in {
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

  def compile[T](t: Term): T = {
    val source = s"""|{
        |  ${imports}
        |  ${declareCaches()}
        |  ${toScala(t)}
        |}""".stripMargin
    println(s"source:\n$source")
    evalScala(source).asInstanceOf[T]
  }

  // compile programs in a shared context,
  // return right-biased nested tuples
  def compileSeq[T](t: Term, ts: Term*): T =
    compile[T](toNestedPairs(t, ts: _*))

  def toNestedPairs(t: Term, ts: Term*): Term =
    if (ts.isEmpty)
      t
    else
      Pair ! t ! toNestedPairs(ts.head, ts.tail: _*)

  it should "produce code that compiles to well-typed Scala" in {
    val t = asTerm(('x % IntType) ->: ('y % IntType) ->: (PlusInt('x, 'y)))
    compileSeq[Any](transform(t), memoizedDerive(t))
  }

  it should "produce correct results for multivariate functions" in {
    import abelianGroups.Library._

    type DInt = Either[(AbelianGroup[Int], Int), Int]

    val t0 = asTerm(('x % IntType) ->: ('y % IntType) ->: (PlusInt('x, 'y)))
    val t  = transform(t0)
    val dt = memoizedDerive(t0)

    // run t to compute 1 + 2 & save results in the cache
    val x0   = 1
    val y0   = 2
    val run0 = asTerm(t ! x0 ! y0)
    val val0 = 3

    // build a term that runs dt with replacement changes
    def runWithChange(newX: Int, newY: Int): Term =
      (updateTerm(IntType) !
        (dt ! x0 ! (replacementChange ! newX) ! y0 ! (replacementChange ! newY)) !
        val0)


    val (delta1, delta2) = (10, 100)

    // run dt with x and y unchanged
    val run1 = runWithChange(x0, y0)
    val val1 = val0

    // run dt with x changed and y unchanged
    val run2 = runWithChange(x0 + delta1, y0)
    val val2 = val0 + delta1

    // run dt with x unchanged and y changed
    val run3 = runWithChange(x0, y0 + delta2)
    val val3 = val0 + delta2

    // run dt with x and y changed
    val run4 = runWithChange(x0 + delta1, y0 + delta2)
    val val4 = val0 + delta1 + delta2

    val (res0, (res1, (res2, (res3, res4)))) =
      compileSeq[(Int, (Int, (Int, (Int, Int))))](run0, run1, run2, run3, run4)

    assert(res0 == val0)
    assert(res1 == val1)
    assert(res2 == val2)
    assert(res3 == val3)
    assert(res4 == val4)
  }
}
