package ilc
package feature
package let

import org.scalatest._
import shapeless._
import poly._

class ANormalFormSpec extends FlatSpec {
  def tests(doCSE_ : Boolean, copyPropagation_ : Boolean) {
    val v = new language.Bacchus with feature.let.ANormalFormStateful with integers.ImplicitSyntaxSugar
      with integers.Evaluation with booleans.Evaluation with let.Evaluation
      with inference.LetInference
      with BetaReduction with inference.LetSyntaxSugar with inference.InferenceTestHelper {
      override val doCSE = doCSE_
      override val copyPropagation = copyPropagation_
    }
    import v._

    //Taken from http://matt.might.net/articles/a-normalization/, but was ill-typed!
  /*
  (define t1
   '(let ((id (λ (x) x)))
      (let ((apply (λ (f x) (f x))))
        ((id apply) (id 3)))))
    */
    val test1 =
      letS(
        'id -> ('x ->: 'x),
        'id_i -> ('x ->: 'x),
        'id_i2 -> ('x ->: 'x),
        'apply -> ('f ->: 'x ->: 'f('x))
      ) {
          'id('apply, 'id_i, 'id_i2(3: Term))
        }
  /*
  (define t2
    '(let ([x (let ([y 20]) y)])
       x))
   */
    val test2 =
      let('x, let('y, 20: Term)('y))('x)
  /*
  (define t3
    '(let ([x (if #t 1 2)])
       x))
  */
    val test3 =
      let('x, ifThenElse(True, 1, 2): Term)('x)
    try {
      pretty(test1)
    } catch { case e: inference.Inference#UnificationFailure =>
      println(e.details)
    }
    "\n" + pretty(test1)
    "normalizeTerm(test1)" should s"contain id_i2 iff !doCSE, doCSE = $doCSE, copyPropagation = $copyPropagation" in {
      var contains_id_i2: Boolean = false
      everywhere {
        case t@Var(LiteralName(name), _) =>
          contains_id_i2 |= name == "id_i2"
          t
        case t => t
      }(normalizeTerm(test1))
      assert(contains_id_i2 === !doCSE)
    }
    "normalizeTerm(test2)" should s"be id_i2 iff copyPropagation, doCSE = $doCSE, copyPropagation = $copyPropagation" in {
      assert((normalizeTerm(test2) == (20: Term)) === copyPropagation)
    }
    "normalizeTerm(test3)" should s"not crash, doCSE = $doCSE, copyPropagation = $copyPropagation" in {
      normalizeTerm(test3)
    }
    for ((test, i_) <- Seq(test1, test2, test3).zipWithIndex) {
      val i = i_ + 1
      val testNorm = normalizeTerm(test)
      s"normalizeTerm(test$i)" should s"not alter evaluation results, doCSE = $doCSE, copyPropagation = $copyPropagation" in {
        assert(eval(testNorm) === eval(test))
      }
      s"normalizeTerm(test$i)" should s"produce a normalizable term, doCSE = $doCSE, copyPropagation = $copyPropagation" in {
        val testNormNorm = normalize(testNorm)
        assert(eval(testNormNorm) === eval(testNorm))
      }
    }
  }
  for {
    doCSE <- Seq(false, true)
    copyPropagation <- Seq(false, true)
  } {
    tests(doCSE, copyPropagation)
  }
}
