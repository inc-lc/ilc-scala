/**
 * Provisional evaluator for Atlas to pass tests
 */

package ilc
package language.atlas

import Syntax._

object Evaluation {

  private type map = collection.immutable.Map[Any, Any]
  private val map = collection.immutable.Map

  def eval(t: Term): Any = try {
    evalWithEnv(t, Nil)
  } catch { case err: java.lang.IllegalArgumentException =>
    throw new java.lang.
      IllegalArgumentException(err.getMessage() ++
        "\n in the term\n    " ++ t.toString)
  }

  def evalWithEnv(t: Term, env: List[Any]): Any = try {
    t match {
      case Abs(x, t) =>
        (arg: Any) => evalWithEnv(t, arg :: env)
      case App(s, t) =>
        evalWithEnv(s, env).
          asInstanceOf[Any => Any](evalWithEnv(t, env))
      case Var(i) =>
        env(i) // Index out of bound error = free var
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
    case _: Empty => map.empty[Any, Any]
    case _: Update =>
      (k: Any) => (v: Any) => (m: map) =>
        if (isNeutral(v))
          m - k
        else
          m.updated(k, v)
    case Lookup(keyType, valType) =>
      (k: Any) => (m: map) =>
        if (m.contains(k))
          m(k)
        else
          eval(neutralTerm(valType))
    case Zip(keyType, valType1, valType2) =>
      (f: Any => Any => Any => Any) => (m1: map) => (m2: map) => {
        val m1WithDefault = m1.withDefaultValue(neutral(valType1))
        val m2WithDefault = m2.withDefaultValue(neutral(valType2))
        val keySet = m1.keySet ++ m2.keySet
        val output: map = keySet.map({ key =>
          key -> f(key)(m1WithDefault(key))(m2WithDefault(key))
        })(collection.breakOut)
        output.filter(p => ! isNeutral(p._2))
      }
    case Fold(keyType, valType) =>
      (f: Any => Any => Any => Any) => (z: Any) => (m: map) =>
        m.foldRight(z)((p, b) => f(p._1)(p._2)(b))
  }

  def neutral(iota: Type) = eval(neutralTerm(iota))

  private def isNeutral(value: Any): Boolean = value match {
    case 0 => true
    case false => true
    case m: map => m.isEmpty
    case _ => false
  }
}
