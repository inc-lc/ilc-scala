package ilc
package language.bacchus

/**
 * Test tools for Bacchus
 */

import collection.immutable
import org.scalatest.FunSuite
import ilc.feature.functions.DerivationTools
import ilc.language.Bacchus

trait Tools
extends DerivationTools { self: FunSuite =>
  val calculus = Bacchus
  import calculus._

  // ASSOC-LIST-BASED TESTING TOOLS FOR MAPS
  //
  // code duplication with ilc.language.atlas.Tools.
  // TODO: come up with suitable abstraction
  // on both the syntactic and the semantic domains
  // for re-use across calculi, then see if these
  // testing tools can be unified.

  def assertMap(t: Term, assoc: (Value, Value)*) {
    assertMapVal(eval(t).toMap, assoc: _*)
  }

  def assertMapVal(t: ValueMap, assoc: (Value, Value)*) {
    assert(t === immutable.Map(assoc: _*))
  }
}
