package ilc
package util

trait ExtractorTrait {
  trait Extractor[From, To] {
    def unapply(from: From): Option[To]
  }

  /*
   * Altered from https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/Extractor.scala.
   * This collapses the cost of writing small extractors.
   */
  def extractor[A, B](f: A => Option[B]) = new Extractor[A, B] { def unapply(x: A): Option[B] = f(x) }
}

//This module has no open dependencies, so there's no real need to use traits
//for it.
//Moreover, using a trait is part of your interface and requires clients to be
//recompiled. In a project where recompilation can take 10 minutes, this can be
//a huge problem.
object Extractors extends ExtractorTrait
