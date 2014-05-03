package ilc
package feature
package let

trait ToScala extends Syntax with functions.ToScala {
  override def toUntypedScala(t: Term) = {
    t match {
      case Let(v, exp, body) =>
        //This does not work when v shadows an existing variable that is used in exp.
        //This shows up in one (1!) case in Histogram - that's it.
        //However, normalization-by-evaluation freshens the variable, preventing this bug.
        //XXX: document the exact interfaces, since we have an interaction between slightly different interfaces (nominal vs. something which I hope is Barendregt), in the area of binding.
        s"""${openBrace()}
        |${indentNoNl()}lazy val ${toUntypedScala(v)} = ${toScala(exp)}
        |${indentNoNl()}${toScala(body)}
        |${closeBrace()}""".stripMargin
      case _ => super.toUntypedScala(t)
    }
  }
}
