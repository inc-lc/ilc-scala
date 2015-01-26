package ilc
package feature
package lists

import ilc.util.ExtractorTrait

trait Types extends base.Types with ExtractorTrait {
  case class ListType(elemType: Type) extends Type {
    override def traverse(f: Type => Type): Type = copy(f(elemType))
  }
}
