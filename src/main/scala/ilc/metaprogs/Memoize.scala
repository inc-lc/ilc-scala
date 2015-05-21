package ilc
package metaprogs

/*
 * This file contains protoytype code related to memoization, both the term
 * transformation and the library support code.
 */

import feature._
import collection.mutable

// ***
// Library code {{{
// ***

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

// ***
// Library code }}}
// ***

trait MemoizeBase {
  outer: base.Syntax with analysis.FreeVariables with base.ToScala =>

  //Use custom name to avoid conflicts

  protected val mFreshGen = new base.FreshGen { val syntax: outer.type = outer }
  def freshCacheName() = mFreshGen.freshName("cache")
  def varToScalaMapType(v: Var, argScalaTyp: Option[String]) = {
    val t = toScala(v.getType)
    t match {
      //Have special support for primitives.
      //XXX for now, just Int.
      //Later, allow using LongMap and coercing every other primitive to
      //Long, a bit like in the miniboxing plugin for the Scala compiler.
      case "Int" =>  //XXX: AAAAAAARGH! We want to know if the generated Scala type is a primitive. String matching is a very fragile way of doing that.
        //XXX: Fully qualified classnames are required when using evalScala, because of https://issues.scala-lang.org/browse/SI-6393.
        //But otherwise, I'd shorten the output by mapping `scm` to `scala.collection.mutable` and selecting from `scm`.
        "scala.collection.mutable.LongMap" + argScalaTyp.fold(""){scalaTyp => s"[$scalaTyp]"}
      case _ =>
        "scala.collection.mutable.HashMap" + argScalaTyp.fold(""){scalaTyp => s"[$t, $scalaTyp]"}
    }
  }
  def newCacheEntry(t: Term): CacheEntry = {
    val cacheNameForT = freshCacheName()
    val freeVars = t.freeVariables.toSeq
    val baseScalaType = t.getType
    val scalaType = freeVars.foldRight[String](s"OptCell[${toScala(baseScalaType)}]") {
      (newVar, scalaTyp) =>
        varToScalaMapType(newVar, Some(scalaTyp))
    }
    // In fact, we might want them ordered according to binding position, like with deBrujin levels, if we wanted to share
    // lookups across different subexpressions.
    val ret = new CacheEntry(cacheNameForT, /*baseScalaType, */ freeVars, scalaType)
    //XXX: What happens here if t is duplicated?
    cacheMap(t) = ret
    ret
  }

  // XXX: This should not really survive forever; instead, it appears that this
  // should be reset for each program (which is fragile), or a different mutable
  // one should be created and threaded around for each independent input
  // program.
  val cacheMap = mutable.Map[Term, CacheEntry]()

  case class CacheEntry(val name: Name, /*val type22: Type, */ val freeVariables: Seq[Var], val scalaType: String)
}
trait MemoizeSyntax extends maps.SyntaxSugar with MemoizeBase {
  this: base.ToScala with analysis.FreeVariables =>
  case class Memo(cacheEntry: CacheEntry, updateCache: Boolean) extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("contentType") {
      case contentType => contentType =>: contentType
    }
  }
}

trait MemoizeToScala extends MemoizeSyntax with base.ToScala {
  this: functions.Syntax with analysis.FreeVariables =>

  //This needs to create a special term that used benign side effects.
  /*def createLookup(cacheName: Name, memoizedT: Term, freeVars: List[Var], updateCache: Boolean): Term =
    freeVars.foldLeft(Summon(cacheName): TermBuilder) { case (cache, newVar) =>
      GetOrElseUpdate ! cache ! newVar ! ??? //memoizedT
    }*/


  /*
  //Some intended example output:
  //val v = mutable.Map[Int, mutable.Map[Int, OptCell[Term]]]()
  //v.getOrElseUpdate(1, mutable.Map()).getOrElseUpdate(2, OptCell(???))
  {
    val cache_1: OptCell[Any => Any] = null
    val cache_2: mutable.HashMap[Any, OptCell[Any => Any]] = null

    (cache_1.getOrElseUpdate((x_param => {
      lazy val x = x_param
      (cache_2.getOrElseUpdate(x, OptCell()))
    })))
  }
  */
  //Warning: No Barendregt convention around to save us!
  override def toUntypedScala(t: Term): String = t match {
    case App(Constant(Memo(ce, _), cType), subTerm) =>
      val (lookups, _) = ce.freeVariables.foldRight((s".getOrElseUpdate(${toUntypedScala(subTerm)})", "OptCell()")) ({
        case (newVar, (lookups, initExp)) => (s".getOrElseUpdate(${newVar.getName}, ${initExp})$lookups",
            varToScalaMapType(newVar, None) + "()")
      })
      //ce.name.toString + lookups
      s"(${ce.name.toString}: ${ce.scalaType})${lookups}"
    case _ => super.toUntypedScala(t)
  }

  def addCaches(code: String): String = {
    val cacheDecls = for {
      CacheEntry(name, _, scalaType) <- cacheMap.values
    } yield s"val ${name} = ${scalaType}()"
    cacheDecls.mkString("\n") + "\n" + code
  }

}

trait Memoize extends MemoizeBase {
  //In fact, we should get the output of CSE probably, so we have good reasons to support let.
  outer: ilc.feature.functions.Syntax with base.Derivation with MemoizeSyntax with analysis.FreeVariables with base.ToScala =>

  def memoizedDerive(t: Term): Term = t match {
    case Abs(x, body) =>
      lambdaTerm(x, DVar(x)) { memoizedDerive(body) }

    case App(operator, operand) =>
      //XXX: What happens here if we have two instances of the same term around?
      val memoizedOperand: Term = Memo(cacheMap(operand), updateCache = false) ! operand
      memoizedDerive(operator) !
        //operand ! // In non-memoizing derivation
        memoizedOperand ! // Main (only) change from non-memoizing derivation!
        memoizedDerive(operand)

    case v: Var =>
      DVar(v)

    // For all terms we don't know how to derive,
    // we produce a derivative that does recomputation.
    // This makes adding new constants easy.
    case _ =>
      Diff ! t ! t
  }

  def doTransform(t: Term, freeVars: List[Var], m: Map[Term, Name] = Map.empty): Term = {
    val cacheEntry = newCacheEntry(t)

    /*
    val memoizedSubterms: Term = t match {
      //XXX add support for Let here.
      case App(s, t) => App(doTransform(s, freeVars), doTransform(t, freeVars))
      case Abs(x, t) => Abs(x, doTransform(t, x :: freeVars))
      //case x: Var => x
      case x => x
    }

    Memo(cacheEntry) ! memoizedSubterms
    */
    t match {
      //XXX add support for Let here.
      case App(s, t) =>
        Memo(cacheEntry, updateCache = true) !
          App(doTransform(s, freeVars), doTransform(t, freeVars))
      case Abs(x, t) =>
        //XXX This will memoize each function in a chain of nested lambdas.
        Memo(cacheEntry, updateCache = true) !
          Abs(x, doTransform(t, x :: freeVars))
      //case x: Var => x
      //Otherwise, for atoms (variables and constants), do *no* memoization.
      case x => x
    }

  }

  def transform(t: Term) = doTransform(t, List())
}
