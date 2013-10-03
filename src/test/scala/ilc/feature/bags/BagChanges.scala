package ilc
package feature
package bags

/** bag changes for generated bags */

trait BagChanges {
  import Library._
  import abelianGroups.Library._

  type ChangeToBags[T] =
    Either[(AbelianGroup[Bag[T]], Bag[T]), Bag[T]]

  def changesToBagsOfIntegers: Map[String, Int => ChangeToBags[Int]] =
    Map("no change"          -> (n => mkChange(bagEmpty)),
        "replace 1 by n + 1" -> (n => replace(1, n + 1)),
        "add n + 2"          -> (n => add(n + 2)),
        "remove 2"           -> (n => remove(2)),
        "remove n"           -> (n => remove(n)))

  private def mkChange[T](element: Bag[T]): ChangeToBags[T] =
    Left((FreeAbelianGroup.apply[T], element))

  private def add[T](e: T) =
    mkChange(bagSingleton(e))

  private def remove[T](e: T) =
    mkChange(bagNegate(bagSingleton(e)))

  private def replace[T](a: T, b: T) =
    mkChange(bagUnion(bagNegate(bagSingleton(a)))(bagSingleton(b)))
}
