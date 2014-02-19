package ilc.feature.inference

import scala.language.postfixOps

/*
In Scala, given the definition of an AST made of case classes, how do you define local transformations without
boilerplate for each case class? Here's my solution, based on reflection.
   -- Paolo
https://gist.github.com/Blaisorblade/827e357de942a46acbdb
 */

object Util {
  def count(amount: Int, noun: String): String = {
    (noun, amount) match {
      case (_, 1) => s"${amount} ${noun}"
      case (_, _) => s"${amount} ${noun}s"
    }
  }
}

import Util._

trait Reflection {
  /**
   * Allow functional update on arbitrary case classes. Call it with an
   * instance of a case class and its updated children.
   * Only works on case classes, or classes having an appropriate copy method.
   * The case class
   * @param t an instance of a case class
   */
  def reflectiveCopy[T <: Product](t: T, args: Any*): T = {
    val clazz = t.getClass
    if (args.length == t.productArity) {
      val copyMethodOpt = clazz.getMethods filter (_.getName == "copy") headOption

      (copyMethodOpt getOrElse (
        throw new RuntimeException("No 'copy' method found in reflectiveCopy")) invoke (t,
        args.toArray.asInstanceOf[Array[_ <: AnyRef]]: _*)).asInstanceOf[T]
    } else {
      throw new IllegalArgumentException(s"${count(t.productArity, "argument")} expected but ${args.length} given")
    }
  }
}
