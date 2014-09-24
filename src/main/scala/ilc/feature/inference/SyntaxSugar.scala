package ilc
package feature
package inference

trait SyntaxSugar extends PrettySyntax with booleans.Syntax {
  //Basic utils.
  //Force implicit conversions.
  def asTerm(t: Term) = t
  def asUntyped(t: UntypedTerm) = t

  def typecheck(t: UntypedTerm) =
    try {
      asTerm(t)
    } catch { case e: inference.Inference#UnificationFailure =>
      println(e.details)
      throw e
    }

  //"Macros"
  /** Usage:
    * {{{
    * let_x_= {
    *   stuff
    * } { x =>
    *   blah blah x blah
    * }
    * }}}
    */
  def let(v: Symbol, meaning: UntypedTerm)
         (body: UntypedTerm): UntypedTerm =
    (v ->: body)(meaning)

  def letS(pairs: (Symbol, UntypedTerm)*)
          (body: UntypedTerm): UntypedTerm = {
    pairs.foldRight(body){ (pair, body) =>
      let(pair._1, pair._2)(body)
    }
  }

  //XXX Should use a macro version of const_.
  def ifThenElse_(condition: UntypedTerm, thenBranch: UntypedTerm, elseBranch: UntypedTerm) =
    asUntyped(IfThenElse)(condition, const_(thenBranch), const_(elseBranch))

  //"Libraries"
  val const_ = asUntyped('a ->: 'b ->: 'a)
}

trait LetSyntaxSugar extends SyntaxSugar with LetUntypedSyntax {
  override def let(v: Symbol, meaning: UntypedTerm)
         (body: UntypedTerm): UntypedTerm =
    ULet(v.name, meaning, body)
}
