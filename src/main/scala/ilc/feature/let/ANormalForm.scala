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
