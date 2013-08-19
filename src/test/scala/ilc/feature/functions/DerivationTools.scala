package ilc
package feature.functions

/**
 * Testing tools for derivatives
 */

import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException


trait DerivationTools { self: FunSuite =>

  val calculus: Syntax with Derivation
                       with Evaluation
                       with feature.DiffAndApply
  import calculus._

  val defaultDiff: (Term, Term) => Term = (x, y) => diffTerm(x)(y)
  val defaultDerive: Term => Term = calculus.derive

  // assert the correctness of the derivative of t
  // according to
  //
  //     f x ⊕ derive(f) x (y ⊝ x) == f y
  //
  def assertCorrect(f: Term,
                    input: List[(Term, Term)],
                    diff: (Term, Term) => Term =
                      (x, y) => diffTerm(x)(y),
                    transform: Term => Term = calculus.derive) {
    val reversedInput = input.reverse
    val theOld    = assembleOld(f, reversedInput)
    val theNew    = assembleNew(f, reversedInput)
    val theChange = assembleChange(transform(f), reversedInput, diff)
    try {
      assert(eval(applyTerm(theChange)(theOld)) === eval(theNew))
    } catch { case err: TestFailedException =>
      val msg = "wrong derivative:" ++
        "\n     old = " ++ eval(theOld).toString ++
        "\n     new = " ++ eval(theNew).toString ++
        "\n  change = " ++ eval(theChange).toString ++
        "\n  result " ++ err.getMessage() ++
        "\n   old-t = " ++ theOld.toString ++
        "\n   new-t = " ++ theNew.toString
      throw new TestFailedException(msg, err, err.failedCodeStackDepth)
    }
  }

  def assembleChange(dt: Term,
                     reversedInput: List[(Term, Term)],
                     diff: (Term, Term) => Term): Term =
    (reversedInput foldRight dt) {
      case ((z, zNew), currDt) =>
        val dz = diff(zNew, z)
        App(App(currDt, z), dz)
    }

  val assembleOld = assembleAccordingTo(_._1)
  val assembleNew = assembleAccordingTo(_._2)

  def assembleAccordingTo(chooser: ((Term, Term)) => Term):
        (Term, List[(Term, Term)]) => Term =
    (t, reversedInput) => (reversedInput map chooser foldRight t) {
      (duo, currTerm) =>
        App(currTerm, duo)
    }
}
