package ilc
package feature
package fixpoint

import org.scalatest.FunSuite
import org.scalatest.Matchers

import language.bacchus
import util.EvalGenerated

import scala.language.implicitConversions

class FixpointSuite
extends FunSuite
    with Matchers
    with booleans.SyntaxSugar //Should be in bacchus. 
    with booleans.ToScala
    with booleans.Derivation
    with bacchus.Syntax
    with bacchus.ToScala
    with bacchus.BasicDerivation
    with Syntax
    with Derivation
    with ToScala
    with integers.SyntaxSugar
    with integers.ToScala
    with integers.AbelianDerivation
    with EvalGenerated {
  val ℤ = IntType

  // this is intToTerm actually
  override implicit def natToTerm(n: Int): Term = LiteralInt(n)

  def multBase(m: Int, n: Int): Int =
    if (n == 0)
      0
    else
      m + multBase(m, n - 1)

  def powerBase(n: Int, exp: Int): Int =
    if (exp == 0)
      1
    else
      n * powerBase(n, exp - 1)

  def minus(a: TermBuilder, b: TermBuilder) = PlusInt ! a ! (NegateInt ! b)

  val mult = Fix(ℤ =>: ℤ =>: ℤ) ! (lambda(Var("multRec", ℤ =>: ℤ =>: ℤ), Var("m", ℤ), Var("n", ℤ)) {
    case Seq(multRec, m, n) =>
      ifThenElse(Eq(ℤ) ! n ! 0, 0, PlusInt ! m ! (multRec ! m ! minus(n, 1)))
  })

  val power = Fix(ℤ =>: ℤ =>: ℤ) ! (lambda(Var("powerRec", ℤ =>: ℤ =>: ℤ), Var("n", ℤ), Var("exp", ℤ)) {
    case Seq(powerRec, n, exp) =>
      ifThenElse(Eq(ℤ) ! exp ! 0, 1, mult ! n ! (powerRec ! n ! minus(exp, 1)))
  })

  //XXX generalize this.
  def expectToGet(i: Int)(t: => Term) {
    //assert(eval(t) === IntValue(i))
    try { assert(evalGenerated(t) === i) }
    catch {
      case e: Throwable =>
        e.printStackTrace()
        if (e.getMessage != null)
          info(e.getMessage)
        info(pretty(t))
        info(toScala(t))
        fail
    }
  }

  val multValue = evalGenerated(mult).asInstanceOf[( => Int) => (=> Int) => Int]
  test("mult works as intended") {
    for {
      i <- 0 to 4
      j <- 0 to 4
    } {
      multValue(i)(j) should be (i * j)
      //expectToGet(i * j)(mult ! i ! j)
    }
  }

  val powerValue = evalGenerated(power).asInstanceOf[( => Int) => (=> Int) => Int]
  test("power works as intended") {

    //Ahem.... XXX
    powerValue(0)(0) should be (1)
    //expectToGet(1)(power ! 0 ! 0)
    for (i <- 1 to 5)
      powerValue(i)(0) should be (1)
      //expectToGet(1)(power ! i ! 0)
    for {
      base <- 1 to 4
      exp <- 1 to 4
    } {
      //expectToGet(powerBase(base, exp))(power ! base ! exp)
      powerValue(base)(exp) should be (powerBase(base, exp))
      powerBase(base, exp) should be (math.pow(base, exp))
    }
  }
  
  test("derive mult") {
    val derivedMult = derive(mult)
    info(pretty(derivedMult))
    val createChange = lambda(ℤ) { n => groupBasedChange ! additiveGroupOnIntegers ! n }
    val derivedMultUsable = lambda(ℤ, ℤ, ℤ, ℤ, ℤ) {
      case Seq(m, dm, n, dn, baseRes) =>
        updateTerm(ℤ) ! (derivedMult ! m ! (createChange ! dm) ! n ! (createChange ! dn)) ! baseRes
    }

    val derivedMultValue = evalGenerated(derivedMultUsable).asInstanceOf[( => Int) => (=> Int) => ( => Int) => (=> Int) => (=> Int) => Int]
    for {
      i <- 0 to 4
      di <- 0 to 2
      j <- 0 to 4
      dj <- 0 to 2
    } {
      val baseRes = multValue(i)(j) 
      baseRes should be (i * j)
      val updatedRes = multValue(i + di)(j + dj) 
      updatedRes should be ((i + di) * (j + dj))
      derivedMultValue(i)(di)(j)(dj)(baseRes) should be (updatedRes) 
    }

  }
}