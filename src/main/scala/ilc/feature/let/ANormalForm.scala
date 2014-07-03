package ilc
package feature
package let

/**
 * Implementation of A-normalization, based on http://matt.might.net/articles/a-normalization/.
 * Written in CPS, so stack usage might be a problem. Should that ever happen, trampolining is an alternative.
 * Alternatively, one could implement this using a writer monad, as suggested by Tillmann.
 */
trait ANormalForm extends Syntax with FreeVariables with analysis.Occurrences with Traversals with ToScala with Pretty with IsAtomic {
  outer =>
  val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  def normalizeTerm(t: Term): Term = normalize(t)(identity)
  def normalize(t: Term)(k: Term => Term): Term = t match {
    case Abs(v, body) =>
      k(Abs(v, normalizeTerm(body)))
    case App(operator, operand) =>
      normalizeName(operator) { s =>
        normalizeName(operand) { t =>
          k(App(s, t))
        }
      }
    case Let(variable, exp, body) =>
      normalize(exp) { normalExp =>
        Let(variable, normalExp, normalize(body)(k))
      }
    case _ if isAtomic(t) => k(t)
  }

  def normalizeName(t: Term)(k: Term => Term) = {
    normalize(t) { normalT =>
      if (isAtomic(normalT))
        k(normalT)
      else {
        val newV = fresh("a", normalT.getType)
        Let(newV, normalT, k(newV))
      }
    }
  }
}

trait ANormalFormStateful extends Syntax with FreeVariables with analysis.Occurrences with Traversals with ToScala with Pretty with IsAtomic {
  outer =>
  private val freshGen = new FreshGen { val syntax: outer.type = outer }
  import freshGen._

  import collection.mutable
  //Test how well this works.
  val doCSE = true
  def normalizeTerm(t: Term): Term = {
    val m = mutable.LinkedHashMap.empty[Term, Var]
    val normalT = normalize(t)(m)
    m.foldRight(normalT) {
      case ((term, variable), t) =>
        Let(variable, term, t)
    }
  }
  def normalize(t: Term)(map: mutable.Map[Term, Var]): Term = t match {
    case Abs(v, body) =>
      Abs(v, normalizeTerm(body))
    case App(operator, operand) =>
      val s = normalizeName(operator)(map)
      val t = normalizeName(operand)(map)
      App(s, t)
    case Let(variable, exp, body) =>
      val normalExp = normalizeName(exp, Some(variable))(map)
      normalize(body)(map)
    case _ if isAtomic(t) => t
  }

  def normalizeName(t: Term, boundVar: Option[Var] = None)(map: mutable.Map[Term, Var]) = {
    val normalT = normalize(t)(map)
    if (isAtomic(normalT)
        //Needed, we can't just drop an existing binding.
        && boundVar == None)
      normalT
    else {
      val newV = boundVar getOrElse fresh("a", normalT.getType)
      if (doCSE)
        map getOrElseUpdate (normalT, newV)
      else {
        map += (normalT -> newV)
        newV
      }
    }
  }
}
