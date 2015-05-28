package ilc
package feature
package memoize

import scala.collection.mutable

trait MemoizeBase {
  outer: base.Syntax with analysis.FreeVariables with base.ToScala =>

  //Use custom name to avoid conflicts
  protected val mFreshGen = new base.FreshGen { val syntax: outer.type = outer }
  def freshCacheName() = mFreshGen.freshName("cache")

  def varToScalaMapType(v: Var, argScalaTyp: Option[String]) = {
    val t = toScala(v.getType)

    //Have special support for primitives.
    //To this end, use LongMap and coerce every other primitive to
    //Long, a bit like in the miniboxing plugin for the Scala compiler.
    if (isScalaPrimitive(v.getType))
      //XXX: Because of evalScala and https://issues.scala-lang.org/browse/SI-6393, pay attention modifying these names.
      //The classes are supposed to come from the scala.collection.mutable package.
      "MemoizePrimMap" + argScalaTyp.fold("") { scalaTyp => s"[$scalaTyp]" }
    else
      "MemoizeObjMap" + argScalaTyp.fold("") { scalaTyp => s"[$t, $scalaTyp]" }
  }

  def getOrElseNewCacheEntry(t: Term): CacheEntry = {
    // If a term appears again, since expressions are pure, it is safe (correctness-wise)
    // to reuse the cache.
    // Caveat #1: This would break down if we started reducing the lifetime of caches.
    // Caveat #2: This tests syntactic equality of expressions, which is rather
    // narrow; sharing of caches can be affected by alpha-renaming variables in
    // the source program. Later we might want to do something about that.
    cacheMap.getOrElseUpdate(t, newCacheEntry(t))
  }

  private def newCacheEntry(t: Term): CacheEntry = {
    val cacheNameForT = freshCacheName()
    val freeVars = t.freeVariables.toSeq
    val baseScalaType = t.getType
    val scalaType = freeVars.foldRight[String](s"OptCell[${toScala(baseScalaType)}]") {
      (newVar, scalaTyp) =>
        varToScalaMapType(newVar, Some(scalaTyp))
    }
    // In fact, we might want free variables to be ordered according to binding position, like with deBrujin levels, if we wanted to share
    // lookups across different subexpressions. (That is, if caches mapped variables to subcaches).
    new CacheEntry(cacheNameForT, /*baseScalaType, */ freeVars, scalaType)
  }

  // XXX: This should not really survive forever; instead, it appears that this
  // should be reset for each program (which is fragile), or a different mutable
  // one should be created and threaded around for each independent input
  // program.
  val cacheMap = mutable.Map[Term, CacheEntry]()
  // However, not clearing the cache properly won't hurt correctness, only
  // increase overhead.

  case class CacheEntry(val name: Name, /*val type22: Type, */ val freeVariables: Seq[Var], val scalaType: String)
}
