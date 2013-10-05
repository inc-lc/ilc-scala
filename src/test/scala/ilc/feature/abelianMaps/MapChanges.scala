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

  /** A group constant. It is important to give it to object programs
    * so that their derivatives can take advantage of the group-based
    * changes herein.
    *
    * Hint: The nil change to an abelian group is itself.
    */
  val additiveIntegerGroup =
    GenerativeGroup[Int](x => y => x + y, -_, 0)

  def changesToMapsBetweenIntegers:
      Map[String, AbelianMap[Int, Int] => ChangeToMaps[Int, Int]] =
    Map(
      "no change" -> (m =>
        mkAdditiveChange()
      ),

      "replace min by max + 1" -> (m => {
        val keySet = m.keySet
        val (min, max) = (keySet.min, keySet.max)
        val multiplicity = m(min)
        mkAdditiveChange(
          min       -> (- multiplicity),
          (max + 1) -> multiplicity)
      }),

      "add max + 2" -> (m =>
        mkAdditiveChange((m.keySet.max + 2) -> 1)
      ),

      "remove second key" -> (m => {
        val sndKey = m.keySet.tail.head
        mkAdditiveChange(sndKey -> (- m(sndKey)))
      }),

      "remove max" -> (m => {
        val max = m.keySet.max
        mkAdditiveChange(max -> (- m(max)))
      }))

  private def mkAdditiveChange[K](keyValuePairs: (K, Int)*):
      ChangeToMaps[K, Int] =
    Left((LiftedMapGroup(additiveIntegerGroup),
         AbelianMap(keyValuePairs: _*)))
}
