package ilc
package feature
package bags

trait Library {
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
      (h, posCount) = if (count >= 0) (g, count) else (inv(g), -count)
      e <- Seq.tabulate(posCount)(_ => h)
    } yield e).fold[G](neutral)(Function.uncurried(op))
  }

  def bagNegate[T](b: Bag[T]): Bag[T] = b mapValues (-_)
}
