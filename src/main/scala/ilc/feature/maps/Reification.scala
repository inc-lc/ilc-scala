package ilc
package feature
package maps

trait Reification extends base.Reification with Evaluation {
  override def reify(value: Value, valueType: Type): Term =
    (value, valueType) match {
      case (MapValue(valueMap), MapType(domain, range)) =>
        valueMap.foldRight(EmptyMap of valueType) { (keyValPair, wip) =>
          Update ! reify(keyValPair._1, domain) !
                   reify(keyValPair._2, range ) ! wip
        }

      case _ =>
        super.reify(value, valueType)
  }
}
