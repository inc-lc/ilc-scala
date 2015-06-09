package ilc
package feature
package memoize

trait Syntax extends MemoizeBase {
  this: base.ToScala with analysis.FreeVariables =>
  case class Memo(cacheEntry: CacheEntry, updateCache: Boolean) extends ConstantWith1TypeParameter {
    val typeConstructor = TypeConstructor("contentType") {
      case contentType => contentType =>: contentType
    }
  }
}
