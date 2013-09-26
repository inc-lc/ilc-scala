package ilc
package feature
package maps

/** Replacement values for non-function types (here: MapType):
  * Every change is a sum, and the right-injection (Inj2) is
  * a replacement value.
  * {{{
  * Δ(Map k v) = Map k (Maybe v ⊎ Δv) ⊎ Map k v
  *                    ins/del  modify  replace
  * }}}
  */

trait ReplacementValuesDerivation
extends base.Derivation
   with Syntax
   with functions.Syntax
   with maybe.Syntax
   with sums.Syntax
{
  override def deltaType(tau: Type): Type = tau match {
    case MapType(k, v) =>
      SumType(surgeryMapType(tau), MapType(k, v))

    case _ =>
      super.deltaType(tau)
  }

  private[this]
  def surgeryMapType: Type => Type = {
    case MapType(k, v) =>
      MapType(k, surgeryValueType(v))
  }

  private[this]
  def surgeryValueType(v: Type): Type =
    SumType(MaybeType(v), deltaType(v))

  object MapSurgery {
    sealed trait Change
    case class DELETE(key: Term) extends Change
    case class INSERT(key: Term, value: Term) extends Change
    case class MODIFY(key: Term, deltaValue: Term) extends Change
  }

  def mkSurgicalMapChange(keyType: Type, valueType: Type)
    (changes: MapSurgery.Change*): Term =
  {
    import MapSurgery._
    def loop(changes: Seq[Change]): Term =
      if (changes.isEmpty)
        EmptyMap(keyType, surgeryValueType(valueType))
      else {
        val (surgeryKey, surgeryValue) = mkSurgeryFrom(changes.head)
        Update ! surgeryKey ! surgeryValue ! loop(changes.tail)
      }

    def mkSurgeryFrom(change: Change): (Term, Term) = {
      val SumType(indelType, modType) = surgeryValueType(valueType)
      val indel = Inj1(modType)
      val mod   = Inj2(indelType)
      change match {
        case DELETE(key) =>
          (key, indel ! Nope(valueType))

        case INSERT(key, value) =>
          (key, indel ! (Just ! value))

        case MODIFY(key, deltaValue) =>
          (key, mod ! deltaValue)
      }
    }

    def wrap(surgicalChanges: Term): Term =
      Inj1(MapType(keyType, valueType)) ! surgicalChanges

    wrap(loop(changes))
  }

  def mkMapReplacement(newMapBuilder: TermBuilder): TermBuilder =
    context => {
      val newMap = newMapBuilder(context).toTerm
      (Inj2(surgeryMapType(newMap.getType)) ! newMap)(context)
    }

  override def updateTerm(tau: Type): Term = tau match {
    case mapType@MapType(k, v) =>
      lambda(deltaType(mapType), mapType) { case Seq(deltaMap, oldMap) =>
        Either !
          // insert, delete, modify
          (Fold !
            lambda(k, surgeryValueType(v), mapType) {
              case Seq(key, insDelMod, wipMap) =>
                Either !
                  (Maybe !
                    (Delete ! key ! wipMap) !
                    lambda(v) { value => Update ! key ! value ! wipMap }) !
                  lambda(deltaType(v)) { valueChange =>
                    Maybe !
                      // if a modification doesn't appear in old map,
                      // ignore it.
                      wipMap !
                      lambda(v) { oldValue =>
                        Update ! key !
                        (updateTerm(v) ! valueChange ! oldValue) !
                        wipMap
                      } !
                      (Lookup ! key ! oldMap)
                  } !
                  insDelMod
            } !
            oldMap) !
          // replace
          lambda(mapType) { x => x } !
          deltaMap
      }

    case _ =>
      super.updateTerm(tau)
  }

  override def diffTerm(tau: Type): Term = tau match {
    case MapType(keyType, valueType) =>
      lambda(tau, tau) { case Seq(newMap, oldMap) =>
        mkMapReplacement(newMap)
      }

    case _ =>
      super.diffTerm(tau)
  }

  override def derive(t: Term): Term = t match {
    case EmptyMap(keyType, valType) =>
      mkSurgicalMapChange(keyType, valType)()

    // slow derivatives for Update, Delete, Lookup, Fold
    case _ =>
      super.derive(t)
  }
}
