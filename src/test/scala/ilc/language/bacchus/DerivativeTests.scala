package ilc
package language
package bacchus

import org.scalatest.FunSuite
import ilc.feature._


/** Tests for all kinds of derivations
  *
  * Examples:
  * {{{
  * // test the correctness of basic derivation
  * class BacchusBasicDerivationSuite
  * extends DerivationTests
  *    with BasicDerivation
  *
  * // test the correctness of optimized derivation
  * class BacchusOptimizedDerivationSuite
  * extends DerivationTests
  *    with OptimizedDerivation
  * }}}
  */

trait DerivativeTests
extends FunSuite
   with Subjects
   with Evaluation
   with Reification
{
  this: ilc.feature.base.Derivation =>

  def valueDiff(minuend: Value, subtrahend: Value): Value =
    (minuend, subtrahend) match {
      case (NatValue(n1), NatValue(n2)) =>
        NatValue(n1)

      case (MapValue(m1), MapValue(m2)) => {
        val toDelete = m2.keySet -- m1.keySet
        val toInsert = m1 -- m2.keySet
        val toUpdate = (m1.keySet & m2.keySet) filter {
          key => m1(key) != m2(key)
        }
        SumValue(Left(
          toInsert.map {
            case (key, value) =>
              key -> SumValue(Left(MaybeValue(Some(value))))
          } ++ toDelete.map {
            key =>
              key -> SumValue(Left(MaybeValue(None)))
          } ++ toUpdate.map {
            key =>
              key -> SumValue(Right(valueDiff(m1(key), m2(key))))
          }
        ))
      }
    }

  def fineGrainedDiff(minuend: Term, subtrahend: Term): Term = {
    val theType = minuend.getType
    require(theType == subtrahend.getType)
    val (mValue, sValue) = (eval(minuend), eval(subtrahend))
    mValue match {
      case _: MapValue | _: NatValue =>
        reify(valueDiff(mValue, sValue), deltaType(theType))

      case _ =>
        Diff ! minuend ! subtrahend
    }
  }

  def assembleTerm(operator: Term, operands: Seq[Term]): Term =
    if (operands.isEmpty)
      operator
    else
      assembleTerm((operator ! operands.head), operands.tail)

  def assertCorrect(t: Term, args: ChangingTerms*) {
    val oldTerm = assembleTerm(t, args.map(_.oldTerm))

    val newOutput = eval(assembleTerm(t, args.map(_.newTerm)))

    val replacement = assembleTerm(derive(t), args flatMap { terms =>
      List(terms.oldTerm, (Diff ! terms.newTerm ! terms.oldTerm).toTerm)
    })

    val surgery = assembleTerm(derive(t), args flatMap { terms =>
      List(terms.oldTerm, fineGrainedDiff(terms.newTerm, terms.oldTerm))
    })

    List(replacement, surgery) foreach { changeTerm =>
      assert(eval(ChangeUpdate ! changeTerm ! oldTerm) === newOutput)
    }
  }

  test("diff and apply works on functions") {
    val f = Plus ! 25
    val g = Plus ! 100
    assert(eval(ChangeUpdate ! (Diff ! g ! f) ! f ! 20).toNat === 120)
  }

  test("[s ⊕ (t ⊝ s) == t] holds for maybe, sums, numbers and maps") {
    def applyDiff(s: Term, t: Term): Value =
      eval(ChangeUpdate ! (Diff ! t ! s) ! s)
    val ff = Nope(ℕ)
    val tt = Just ! 0
    val ll = Inj1(ℕ) ! 0
    val rr = Inj2(ℕ) ! 0
    List.apply[ChangingTerms](
      ff ↦ ff, ff ↦ tt, tt ↦ ff, tt ↦ tt,
      ll ↦ ll, ll ↦ rr, rr ↦ ll, rr ↦ rr,
      392 ↦ 1522,
      (sum ! oldMap) ↦ (sum ! newMap),
      oldMap ↦ newMap,
      mapLiteral(oldMap -> EmptyMap(ℕ, ℕ),
                 newMap -> oldMap,
                 mapLiteral(99 -> 217) -> mapLiteral(2012 -> 56)) ↦
        mapLiteral(oldMap -> EmptyMap(ℕ, ℕ),
                   newMap -> newMap,
                   EmptyMap(ℕ, ℕ) -> newMap)
    ).foreach { case ChangingTerms(oldTerm, newTerm) =>
      assert(applyDiff(oldTerm, newTerm) === eval(newTerm))
    }
  }

  test("the derivative of constants are nil changes of themselves") {
    def assertNil(t: Term): Unit =
      assert(eval(ChangeUpdate ! derive(t) ! t) === eval(t))

    List.apply[Term](
      Nat(5), EmptyMap(ℕ, ℕ), Inj1(ℕ) ! EmptyMap(ℕ, ℕ), Inj2(ℕ) ! 5
    ).foreach(assertNil)
  }

  test("the derivative of Plus is correct") {
    (natPairs, natPairs.reverse).zipped.foreach { (p1, p2) =>
      val ((x, xNew), (yNew, y)) = (p1, p2)
      assertCorrect(Plus, x ↦ xNew, y ↦ yNew)
    }
  }

  test("the derivative of FoldNat is correct") {
    val args2 = List(5 ↦ 1997, (Plus ! 25) ↦ (Plus ! 100))
    val args3 = args2 ++ List(40 ↦ 5)

    assertCorrect(FoldNat(ℕ), args3: _*)

    val foldNatWithFixedIterations: Term =
      lambda(ℕ, ℕ =>: ℕ) { case Seq(z, f) => FoldNat ! z ! f ! 25 }

    assertCorrect(foldNatWithFixedIterations, args2: _*)
  }

  test("the derivative of Update is correct") {
    // stable keys and values
    keyCases foreach { key =>
      assertCorrect(Update ! key ! 18, oldMap ↦ newMap)
    }

    // stable keys
    keyCases foreach { key =>
      assertCorrect((Update ! key) % ℕ, 10 ↦ 18, oldMap ↦ newMap)
    }

    // changing everything
    for {
      oldKey <- keyCases
      newKey <- keyCases
    } {
      assertCorrect(Update(ℕ, ℕ), oldKey ↦ newKey, 10 ↦ 18, oldMap ↦ newMap)
    }
  }

  test("the derivative of Delete is correct") {
    // stable key & map
    assertCorrect(Delete ! 4 ! oldMap)
    assertCorrect(Delete ! 5 ! oldMap)

    // stable key
    List(1, 3, 5, 7).foreach { i =>
      assertCorrect(Delete(ℕ, ℕ) ! i, oldMap ↦ newMap)
    }

    // changing everything
    assertCorrect(Delete(ℕ, ℕ), 5 ↦ 5, oldMap ↦ oldMap)
    assertCorrect(Delete(ℕ, ℕ), 5 ↦ 5, oldMap ↦ newMap)
    assertCorrect(Delete(ℕ, ℕ), 5 ↦ 7, oldMap ↦ newMap)
  }

  test("the derivative of Fold is correct") {
    val plusValue = lambda(ℕ) { x => Plus }

    // constant f z
    assertCorrect(Fold ! plusValue ! 0, oldMap ↦ newMap)

    // constant f
    assertCorrect(Fold ! plusValue, 0 ↦ 100, oldMap ↦ newMap)

    // changing everything
    assertCorrect(Fold(ℕ, ℕ, ℕ),
      plusValue ↦ lambda(ℕ, ℕ, ℕ) {
        case Seq(k, x, y) => Plus ! (Plus ! 2000 ! x) ! y
      },
      0 ↦ 100,
      oldMap ↦ newMap)
  }
}
