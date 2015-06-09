package ilc
package feature
package let

trait Traversals extends Syntax {
  type =?>:[A, B] = PartialFunction[A, B]

  //Switch to Scalaz?
  //Does not work.
  //def or[T, U](f: T =?>: U)(g: T => U): T => U =
  //Works.
  def or[T, U](f: PartialFunction[T, U])(g: T => U): T => U =
    x => f applyOrElse (x, g)

  def orIdentity[T](f: T =?>: T): T => T =
    or(f)(identity)

  //Switch to shapeless?
  //Probably yes, since I had to debug this, and it needs to be extended for Let (as lots of existing code), and so on.
  /**
    * Transform a Term by traversing it in post-order.
    * The transformer argument receives a tree where children have been
    * transformed.
    */
  def everywhere: (Term => Term) => (Term => Term) =
    transf => term =>
      transf(term match {
        case App(f, t) => App(everywhere(transf)(f), everywhere(transf)(t))
        case Abs(v, body) => Abs(v, everywhere(transf)(body))
        case Let(v, exp, body) => Let(v, everywhere(transf)(exp), everywhere(transf)(body))
        case other =>
          other
      })

  /**
    * Transform a Term by traversing it in post-order.
    * The transformer argument receives a tree where children have been
    * transformed, <em>together with the original tree</em> (which is instead
    * not passed by <code>everywhere</code>).
    */
  def everywherePrePost: ((Term, Term) => Term) => (Term => Term) =
    transf => term =>
      transf(term, term match {
        case App(f, t) => App(everywherePrePost(transf)(f), everywherePrePost(transf)(t))
        case Abs(v, body) => Abs(v, everywherePrePost(transf)(body))
        case Let(v, exp, body) => Let(v, everywherePrePost(transf)(exp), everywherePrePost(transf)(body))
        case other =>
          other
      })
}
