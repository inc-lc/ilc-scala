package ilc
package feature
package bags

trait Library {
  type Bag[T] = Map[T, Int]

  def bagEmpty[T]: Bag[T] = Map.empty[T, Int]

  def bagSingleton[T](t: T): Bag[T] = Map(t -> 1)

  //XXX: Could be made much faster by using builders (avoiding immutable
  //copies), but this shouldn't be necessary.
  def bagUnion[T](b1: Bag[T])(b2: Bag[T]) =
    b2 ++ (b1 flatMap {
      case (el, count) =>

      val newCount =
        if (b1 contains el)
          count + b1(el)
        else
          count
      if (newCount == 0)
        List.empty
      else
        List(el -> newCount)
    })

  def bagFoldGroup[G, T](op: G => G => G)(inv: G => G)(neutral: G)(f: T => G)(bag: Bag[T]): G = {
    (for {
      (t, count) <- bag
      g = f(t)
      (h, posCount) = if (count >= 0) (g, count) else (inv(g), -count)
      e <- Seq.tabulate(posCount)(_ => h)
    } yield e).foldRight[G](neutral)(Function.uncurried(op))
  }

  def bagNegate[T](b: Bag[T]): Bag[T] = b mapValues (-_)
}
