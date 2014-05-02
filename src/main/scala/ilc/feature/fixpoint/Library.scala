package ilc
package feature
package fixpoint

object Library extends base.Library {
  def fix[T]: (=> (=> T) => T) => T =
    fParam => {
      lazy val f = fParam
      //Or fParam?
      f(fix(f))
    }
}
