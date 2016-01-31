package ilc
package feature
package base

import scala.language.implicitConversions
import scala.language.postfixOps

/**
 * A reusable freshname generator. Since this contains mutable state, this module
 * is designed to be imported via composition, not by mixing it in.
 */
trait FreshGen {
  val syntax: Syntax
  import syntax._

  //Have a very simple and reliable fresh variable generator. Tracking free
  //variables might have been the performance killer of the other normalizer.
  var index = 0
  def resetIndex() {
    index = 0
  }

  def freshName(varName: Name): Name = {
    index += 1
    val compress = false
    val baseName =
      if (compress)
        "z": NonIndexedName
      else
        //Keep names, for extra readability.
        decomposeName(varName)._1
    //IndexedName("z", index)
    IndexedName(baseName, index)
  }

  def fresh(varName: Name, varType: Type): Var = {
    Var(freshName(varName), varType)
  }

  def fresh(v: Var): Var = fresh(v.getName, v.getType)
}
