package ilc
package examples

trait MapSuccTypes {
  type Data = Map[Int, Int]
  type Change = Either[
    Map[Int, Either[Either[Unit, Int], Either[Unit, Int]]],
    Map[Int, Int]]

  def applyChange(dm: Change, m: Data): Data = dm match {
    case Right(replacement) =>
      replacement

    case Left(delInsMod) =>
      delInsMod.foldRight(m) { (keyMod, theMap) =>
        keyMod match {
          // deletion
          case (key, Left(Left(()))) =>
            theMap - key

          // insertion
          case (key, Left(Right(value))) =>
            theMap.updated(key, value)

          // change (change application of naturals inlined)
          case (key, Right(Right(replacementValue))) =>
            theMap.updated(key, replacementValue)

          case _ =>
            sys error "Left(Unit) encountered as change to Nat"
        }
      }
  }
}
