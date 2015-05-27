package ilc
package feature
package memoize

trait ToScala extends Syntax with base.ToScala {
  this: functions.Syntax with analysis.FreeVariables =>

  addLibrary("memoize")

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

  def toScalaWidened(v: Var): String = {
    val tAsScala = v.getName.toString
    if (isScalaPrimitive(v.getType)) {
      s"widenToLong($tAsScala)"
    } else {
      tAsScala
    }
  }

  //Warning: No Barendregt convention around to save us!
  override def toUntypedScala(t: Term): String = t match {
    case App(Constant(Memo(ce, _), cType), subTerm) =>
      val (lookups, _) = ce.freeVariables.foldRight((s".getOrElseUpdate(${toUntypedScala(subTerm)})", "OptCell()")) ({
        case (newVar, (lookups, initExp)) => (s".getOrElseUpdate(${toScalaWidened(newVar)}, ${initExp})$lookups",
            varToScalaMapType(newVar, None) + "()")
      })
      //ce.name.toString + lookups
      s"(${ce.name.toString}: ${ce.scalaType})${lookups}"
    case _ => super.toUntypedScala(t)
  }

  def declareCaches(): String = {
    val cacheDecls = for {
      CacheEntry(name, _, scalaType) <- cacheMap.values
    } yield s"val ${name} = ${scalaType}()"
    cacheDecls.mkString("\n")
  }

}
