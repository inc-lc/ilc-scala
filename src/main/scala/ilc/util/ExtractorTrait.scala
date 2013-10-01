package ilc
package util

trait ExtractorTrait {
  trait Extractor[From, To] {
    def unapply(from: From): Option[To]
  }
}
