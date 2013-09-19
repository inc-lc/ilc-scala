package ilc
package language
package bacchus

import feature._

trait FineGrainedDifference
extends maps.Reification
   with maybe.Reification
   with naturals.Reification
   with sums.Reification
   with base.Derivation
{
  private[this] def valueDiff(minuend: Value, subtrahend: Value): Value =
    (minuend, subtrahend) match {
      case (NatValue(n1), NatValue(n2)) =>
        NatValue(n1)

      case (MapValue(m1), MapValue(m2)) => {
        val toDelete = m2.keySet -- m1.keySet
        val toInsert = m1 -- m2.keySet
        val toUpdate = (m1.keySet & m2.keySet) filter {
          key => m1(key) != m2(key)
        }
        SumValue(Left(MapValue(
          toInsert.map {
            case (key, value) =>
              key -> SumValue(Left(MaybeValue(Some(value))))
          } ++ toDelete.map {
            key =>
              key -> SumValue(Left(MaybeValue(None)))
          } ++ toUpdate.map {
            key =>
              key -> SumValue(Right(valueDiff(m1(key), m2(key))))
          }
        )))
      }
    }

  def fineGrainedDiff(minuend: Term, subtrahend: Term): Term = {
    val theType = minuend.getType
    require(theType == subtrahend.getType)
    val (mValue, sValue) = (eval(minuend), eval(subtrahend))
    mValue match {
      case _: MapValue | _: NatValue =>
        reify(valueDiff(mValue, sValue), deltaType(theType))

      case _ =>
        Diff ! minuend ! subtrahend
    }
  }
}
