package ilc
package metaprogs

import feature._
import org.scalatest._
import org.scalatest.matchers._

/**
 * @author pgiarrusso
 */
class CachingDeriveSuite extends FlatSpec with Matchers {
  val v = new CachingDerive {
    val mySyntax = new language.LetLanguage
  }
  import v._
  import mySyntax._

  "cachingDerivation" should "produce well-typed terms" in {
    val t =
      //letS('v := 1)('v)
      letS('v := 'x % IntType ->: 'x)('v)
    println(pretty(cacheDerive(t)))
  }
}
