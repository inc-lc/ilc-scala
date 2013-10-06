package ilc
package feature
package abelianMaps

/** changes for generated maps */

trait MapChanges {
  import Library._
  import abelianGroups.Library._

  type ChangeToMaps[K, V] =
    Either[(AbelianGroup[AbelianMap[K, V]], AbelianMap[K, V]),
           AbelianMap[K, V]]

  def changesToMapsBetweenIntegers:
      Map[String,
          AbelianGroup[Int] => AbelianMap[Int, Int] =>
            ChangeToMaps[Int, Int]] =
    Map(
      "no change" -> (grp => m =>
        mkAdditiveChange(grp)()
      ),

      "replace min by max + 1" -> (grp => m => {
        val keySet = m.keySet
        val (min, max) = (keySet.min, keySet.max)
        val multiplicity = m(min)
        mkAdditiveChange(grp)(
          min       -> (- multiplicity),
          (max + 1) -> multiplicity)
      }),

      "add max + 2" -> (grp => m =>
        mkAdditiveChange(grp)((m.keySet.max + 2) -> 1)
      ),

      "remove second key" -> (grp => m => {
        val sndKey = m.keySet.tail.head
        mkAdditiveChange(grp)(sndKey -> (- m(sndKey)))
      }),

      "remove max" -> (grp => m => {
        val max = m.keySet.max
        mkAdditiveChange(grp)(max -> (- m(max)))
      }))

  private def mkAdditiveChange[K]
                (grp: AbelianGroup[Int])
                (keyValuePairs: (K, Int)*): ChangeToMaps[K, Int] =
    Left((LiftedMapGroup(grp), AbelianMap(keyValuePairs: _*)))
}
