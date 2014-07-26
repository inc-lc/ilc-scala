package ilc
package language

import gcc._
import feature._

/**
  * A front-end language for the GCC assembler from ICFP Contest 2014.
  * http://icfpcontest.org/specification.html#lambda-man-cpu
  *
  * Primitive data types: integers, closures, cons cells.
  * This is untyped.
  *
  * We have untyped pairs, which are used to encode other types (lists and n-ary tuples).
  */
class GCC extends Syntax with Pretty with ToProcessor
  with inference.PrettySyntax

object GCC extends GCC
