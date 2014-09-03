package ilc
package feature
package functions

trait Types extends base.Types {
  typesTrait =>
  /**
   * We will be writing function types a lot, thus the right-
   * associative infix notation. For example, it can be used as
   *
   *   (foo =>: foo) =>: foo =>: foo
   *
   * to create the type of Church numerals for base type foo.
   */
  case class =>: (domain: Type, range: Type) extends Type {
    override def toString: String = {
      def arrow = =>:.arrow
      domain match {
        case _ =>: _ => s"($domain) $arrow $range"
        case _ => s"$domain $arrow $range"
      }
    }
    override def traverse(f: Type => Type) = copy(f(domain), f(range))
  }

  object =>: {
    val arrow = UnicodeOutput.choose("→", "->")
  }

  def getArgumentTypes(functionType: Type): List[Type] =
    functionType match {
      case domain =>: range =>
        domain :: getArgumentTypes(range)

      case _ =>
        Nil
    }

  // sugar, so that we may write `NatType =>: NatType =>: NatType`
  // to construct a type. Because the method `=>:` ends with colon,
  // it is right-associative and is invoked on the object on the
  // right hand side.
  //
  // feature description:
  // Scala Language Specification §6.12.3
  //
  // similar code in standard library:
  // http://goo.gl/emIQZf
  //
  implicit class FunctionsTypeOps(range: Type) {
    def =>: (domain: Type): Type =
      new typesTrait.=>:(domain, range)
  }
}
