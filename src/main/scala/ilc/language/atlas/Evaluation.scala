/**
 * Provisional evaluator for Atlas to pass tests
 */

package ilc
package language.atlas

import Syntax._

import collection.immutable

object Evaluation {

  type Env = immutable.Map[String, Any]

  private type AnyMap = immutable.Map[Any, Any]

  def eval(t: Term): Any = try {
    evalWithEnv(t, immutable.Map.empty)
  } catch { case err: java.lang.IllegalArgumentException =>
    throw new java.lang.
      IllegalArgumentException(err.getMessage() ++
        "\n in the term\n    " ++ t.toString)
  }

  def evalWithEnv(t: Term, env: Env): Any = try {
    t match {
      case Abs(x, t) =>
        (arg: Any) => evalWithEnv(t, env.updated(x, arg))
      case App(s, t) =>
        evalWithEnv(s, env).
          asInstanceOf[Any => Any](evalWithEnv(t, env))
      case Var(name) =>
        env(name) // NoSuchElementException = free var
      case Const(c) => evalConst(c)
    }
  } catch { case err: java.lang.ClassCastException =>
    throw new java.lang.
      IllegalArgumentException(err.getMessage() ++
        " when evaluating:\n    " ++ t.toString)
  }

  def evalConst(c: Constant): Any = c match {
    case True => true
    case False => false
    case Xor => (x: Boolean) => (y: Boolean) => {
      (x && ! y) || (! x && y)
    }
    case Num(i) => i
    case Plus => (x: Int) => (y: Int) => x + y
    case Negate => (x: Int) => - x
    case _: Empty => immutable.Map.empty[Any, Any]
    case _: Update =>
      (k: Any) => (v: Any) => (m: AnyMap) =>
        if (isNeutral(v))
          m - k
        else
          m.updated(k, v)
    case Lookup(keyType, valType) =>
      (k: Any) => (m: AnyMap) =>
        if (m.contains(k))
          m(k)
        else
          eval(neutralTerm(valType))
    case Zip(keyType, valType1, valType2, resultType) =>
      (f: Any => Any => Any => Any) => (m1: AnyMap) => (m2: AnyMap) => {
        val m1WithDefault = m1.withDefaultValue(neutral(valType1))
        val m2WithDefault = m2.withDefaultValue(neutral(valType2))
        val keySet = m1.keySet ++ m2.keySet
        val output: AnyMap = keySet.map({ key =>
          key -> f(key)(m1WithDefault(key))(m2WithDefault(key))
        })(collection.breakOut)
        output.filter(p => ! isNeutral(p._2))
      }
    case Fold(keyType, valType) =>
      (f: Any => Any => Any => Any) => (z: Any) => (m: AnyMap) =>
        m.foldRight(z)((p, b) => f(p._1)(p._2)(b))
  }

  def neutral(iota: Type) = eval(neutralTerm(iota))

  private def isNeutral(value: Any): Boolean = value match {
    case 0 => true
    case false => true
    case m: AnyMap => m.isEmpty
    case _ => false
  }
}
