package ilc
package feature
package let

trait Traversals extends Syntax {
  type =?>:[A, B] = PartialFunction[A, B]

  //Switch to Scalaz?
  //Does not work.
  def or[T, U](f: T =?>: U)(g: T => U): T => U =
  //Works.
  //def or[T, U](f: PartialFunction[T, U])(g: T => U): T => U =
    x => f applyOrElse (x, g)

  def orIdentity[T](f: T =?>: T): T => T =
    or(f)(identity)

  //Switch to shapeless?
  //Probably yes, since I had to debug this, and it needs to be extended for Let (as lots of existing code), and so on.
  def everywhere: (Term => Term) => (Term => Term) =
    transf => term =>
      transf(term match {
        case App(f, t) => App(everywhere(transf)(f), everywhere(transf)(t))
        case Abs(v, body) => Abs(v, everywhere(transf)(body))
        case Let(v, exp, body) => Let(v, everywhere(transf)(exp), everywhere(transf)(body))
        case other =>
          other
      })
}
