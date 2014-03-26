package longRunning.evalScala

import ilc.feature.booleans.BooleanSuiteBase
import ilc.util.EvalScala

class BooleanSuiteCompile extends BooleanSuiteBase {
  def expectToGet(b: Boolean)(t: => Term) {
    assert(evalScala(toScala(t)) === b)
  }
}
