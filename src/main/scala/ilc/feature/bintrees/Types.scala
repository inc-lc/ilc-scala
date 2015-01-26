package ilc
package feature
package bintrees

import ilc.util.ExtractorTrait

trait Types extends base.Types with ExtractorTrait {
  case class BinTreeType(elemType: Type) extends Type {
    override def traverse(f: Type => Type): Type = copy(f(elemType))
  }
}
