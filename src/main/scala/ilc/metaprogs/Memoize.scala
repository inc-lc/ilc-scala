package ilc
package metaprogs

/*
 * This file contains protoytype code related to memoization, both the term
 * transformation and the library support code.
 */

import feature._
import collection.mutable

//This should be in some standard library — it's a thread-unsafe version of scala.concurrent.SyncVar
class Cell[T](initialContent: T) {
  var value: T = initialContent
  def apply() = value
  def update(newValue: T) {
    value = newValue
  }
}

class OptCell[T] {
  val box: Cell[Option[T]] = new Cell(None)
  def getOrElseUpdate(newValue: => T): T = {
    val curr = box()
    curr match {
      case Some(v) => v
      case None =>
        val ret = newValue
        box() = Some(ret)
        ret
    }
  }
}

object OptCell {
  def apply[T](initialContent: T): OptCell[T] = {
    val ret = new OptCell[T]
    ret.box() = Some(initialContent)
    ret
  }
  def apply[T]() = new OptCell[T]
}


//We need identity hash maps here!!!
case class DefaultedMap[K, V]() extends mutable.HashMap[K, V] {

}

trait MemoizeSyntax extends maps.SyntaxSugar {
  //Mutable maps.
  case class Summon(n: Name) extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("contentType") {
      case contentType => contentType
    }
  }

  case class Memo(n: Name) extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("contentType") {
      case contentType => contentType =>: contentType
    }
  }

  /*
  case object GetOrElseUpdate extends ConstantWith2TypeParameters {
    val typeConstructor = TypeConstructor("keyType", "valueType") {
      case Seq(keyType, valueType) => MapType(keyType, valueType) =>: keyType =>: valueType =>: valueType
    }
  }
  */
  //def doLookup(cache: Term)
}

trait MemoizeToScala extends MemoizeSyntax with base.ToScala {
  this: functions.Syntax with analysis.FreeVariables =>
  override def toUntypedScala(t: Term): String = t match {
    case App(Constant(Memo(n), cType), subTerm) =>
      n.toString + subTerm.freeVariables.foldRight((s".getOrElseUpdate(${toUntypedScala(subTerm)})", "OptCell()")) ({
        case (newVar, (lookups, initExp)) => (s".getOrElseUpdate(${newVar.getName}, ${initExp})", "scala.collection.mutable.HashMap()")
      })
    case _ => super.toUntypedScala(t)
  }
}

trait Memoize {
  //In fact, we should get the output of CSE probably, so we have good reasons to support let.

  outerM: ilc.feature.functions.Syntax with base.Derivation with MemoizeSyntax =>

  //Use custom name to avoid conflicts
  protected val mFreshGen = new base.FreshGen { val syntax: outerM.type = outerM }
  def freshCacheName() = mFreshGen.freshName("cache")
  val cacheNameMap = mutable.Map[Term, Name]()

  //Example code:
  {
    val v = mutable.Map[Int, mutable.Map[Int, OptCell[Term]]]()
    v.getOrElseUpdate(1, mutable.Map()).getOrElseUpdate(2, OptCell(???))
  }
  //That's what we want to generate for a lookup.

  //This needs to create a special term that used benign side effects.
  /*def createLookup(cacheName: Name, memoizedT: Term, freeVars: List[Var], updateCache: Boolean): Term =
    freeVars.foldLeft(Summon(cacheName): TermBuilder) { case (cache, newVar) =>
      GetOrElseUpdate ! cache ! newVar ! ??? //memoizedT
    }*/

  def memoizedDerive(t: Term): Term = t match {
    case Abs(x, body) =>
      lambdaTerm(x, DVar(x)) { memoizedDerive(body) }

    case App(operator, operand) =>
      val memoizedOperand: Term = Memo(cacheNameMap(operand)) ! operand
        //operand // In old-style derivation
        //createLookup(operand, cacheNameMap(t)
        //createLookup(cacheNameMap(operand), operand /* ??? or try lookup recursively for failure??? */, ??? /*freeVars*/, updateCache = false)
      memoizedDerive(operator) ! memoizedOperand ! memoizedDerive(operand)

    case v: Var =>
      DVar(v)

    // For all terms we don't know how to derive,
    // we produce a derivative that does recomputation.
    // This makes adding new constants easy.
    case _ =>
      Diff ! t ! t
  }

  def doTransform(t: Term, freeVars: List[Var], m: Map[Term, Name] = Map.empty): Term = {
    val cacheNameForT = freshCacheName()
    cacheNameMap(t) = cacheNameForT

    val memoizedSubterms: Term = t match {
      case App(s, t) => App(doTransform(s, freeVars), doTransform(t, freeVars))
      case Abs(x, t) => Abs(x, doTransform(t, x :: freeVars))
      case x: Var => x
    }

    //createLookup(cacheNameForT, memoizedSubterms, freeVars /* or get only the free variables actually used in the term, instead of the ones from outside? */, updateCache = true)
    Memo(cacheNameForT) ! memoizedSubterms
  }

  def transform(t: Term) = doTransform(t, List())
}
