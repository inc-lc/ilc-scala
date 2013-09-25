package ilc
package feature
package bags

trait BagLibrary {
  type Bag[T] = Map[T, Int]

  def bagEmpty[T]: Bag[T] = Map.empty[T, Int]

  def bagSingleton[T](t: T): Bag[T] = Map(t -> 1)

  def bagUnion[T](b1: Bag[T])(b2: Bag[T]) =
    b1 ++ (for ((el, count) <- b2) yield {
      val newCount =
        if (b1 contains el)
          count + b1(el)
        else
          count
      el -> newCount
    })

  def bagFoldGroup[G, T](op: G => G => G)(inv: G => G)(neutral: G)(f: T => G)(bag: Bag[T]): G = {
    (for {
      (t, count) <- bag
      g = f(t)
      e <- Seq.tabulate(count)(_ => g)
    } yield e).fold[G](neutral)(Function.uncurried(op))
  }

  def bagNegate[T](b: Bag[T]): Bag[T] = b mapValues (-_)
}

trait ToScala extends base.ToScala with Syntax {
  private[this] def mapTypes(v: Type): (String, String) = {
    val vType = toScala(v)
    ("Map[%s, Int]".format(vType), vType)
  }

  override def toScala(t: Term): String =
    t match {
      case EmptyBag(v) =>
        val (mType, vType) = mapTypes(v)
        //s"Map.empty[$mType, Int]"
        s"bagEmpty[$vType]"
      case Singleton(v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagSingleton[$vType] _)"
      case Union(v) =>
        val (mType, vType) = mapTypes(v)
        //s"((b1: $mType) => (b2: $mType) => bagUnion(b1, b2))"
        s"(bagUnion[$vType] _)"
      case Negate(v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagNegate[$vType] _)"
      case FoldGroup(b, v) =>
        val (mType, vType) = mapTypes(v)
        s"(bagFoldGroup[${toScala(b)}, $vType] _)"
      case _ =>
        super.toScala(t)
    }

  override def toScala(tau: Type): String =
    tau match {
      case BagType(valType) =>
        mapTypes(valType)._1

      case _ =>
        super.toScala(tau)
    }
}
