package ilc
package feature
package functions

import scala.language.postfixOps

trait ToScala extends base.ToScala with Syntax {
  final override def toScala(t: Term): String =
    t match {
      /*
       * Type annotations here are not needed.
       * Moreover, for a curried n-parameter function, these type annotations
       * consume O(n^2). The output looks essentially like this:
       * ((...((...)(arg_1): arg2Type => arg3Type => ... argNType => resType)(arg_2):
       * arg3Type => ... argNType => resType)...) (argN) :resType)
       * Say that all those type have size >= k in concrete syntax.
       * Then the space we need is at least k * (n + (n - 1) + (n - 2) + ... + 1),
       * that is o(k * n^2). That's why it's important to save this space.
       */
      case App(f, x) => s"(${toUntypedScala(t)})"
      case v: Var => toUntypedScala(t)
      case _ => super.toScala(t)
    }


  override def toUntypedScala(t: Term): String = t match {
    case App(f, x) =>
      "%s(%s)".format(toScala(f), toScala(x))

    case Abs(variable, bodyTerm) =>
      scalaFunction(variable.getName.toString)(toScala(bodyTerm))

    case _ =>
      super.toUntypedScala(t)
  }

  override def toScala(tau: Type): String = tau match {
    case sigma0 =>: sigma1 => {
      s"((=>${toScala(sigma0)}) => ${toScala(sigma1)})"
    }

    case _ =>
      super.toScala(tau)
  }
}
