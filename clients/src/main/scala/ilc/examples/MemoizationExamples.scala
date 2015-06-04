package ilc
package examples

import java.io.File

import feature._
import metaprogs._

object MemoizationExamples extends Archive { memoizationExamples =>

  // run x + y 4 times, with x and y constant or changing
  object MemoXPlusY extends MemoExample {
    import abelianGroups.Library._
    import memoCtx._

    val program: Term = {
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

      val body = toNestedPairs(
        Pair ! run0 ! val0,
        Pair ! run1 ! val1,
        Pair ! run2 ! val2,
        Pair ! run3 ! val3,
        Pair ! run4 ! val4)

      ('_ % UnitType) ->: body
    }
  }
  addExample(MemoXPlusY)
}

abstract class MemoExample extends Example
    with Memoize
    with memoize.Syntax
    with functions.Derivation
    with integers.ImplicitSyntaxSugar with integers.ToScala with integers.AbelianDerivation
    with functions.Syntax with let.Syntax
    with inference.PrettySyntax with inference.LetSyntaxSugar
    with inference.LetInference
    with let.Pretty with let.Traversals
    with analysis.FreeVariables
    with functions.ToScala with sums.ToScala with abelianGroups.ToScala with products.ToScala with memoize.ToScala
    with unit.ToScala
{
  // global store of caches for subexpressions
  val memoCtx = new MemoContext()


  override def toSource(base: java.io.File): Seq[Source] = Seq(
    Source(this, new File(base, Archive.toGenName(name) + "ProgBase.scala"), () => {
      assert(indentDiff == 2)
      setIndentDepth(2)

      val programCode = addMemoHeader(toScala(program))

      //The output template in toSource relies on this value.
      setIndentDepth(4)

      s"""|package ilc.examples
          |
          |$imports
          |
          |object ${Archive.toGenName(name)} {
          |  val program = $programCode
          |}
          |""".stripMargin
    }))

  // add imports & cache declarations to code
  def addMemoHeader(code: String) =
    s"""|{
        |  ${imports}
        |  ${declareCaches(memoCtx)}
        |  $code
        |}""".stripMargin

  // put terms into right-biased nested tuples
  def toNestedPairs(t: Term, ts: Term*): Term =
    if (ts.isEmpty)
      t
    else
      Pair ! t ! toNestedPairs(ts.head, ts.tail: _*)
}
