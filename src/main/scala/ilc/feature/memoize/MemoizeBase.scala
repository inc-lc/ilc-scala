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
