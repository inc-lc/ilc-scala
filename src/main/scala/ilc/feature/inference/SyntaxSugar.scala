package ilc
package feature
package inference

trait SyntaxSugar extends PrettySyntax with booleans.Syntax {
  //Basic utils.
  def asUntyped(t: UntypedTerm) = t

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

trait LetSyntaxSugar extends PrettySyntax with booleans.Syntax {
  case class ULet(variable: String, exp: UntypedTerm, body: UntypedTerm) extends UntypedTerm
  def let(v: Symbol, meaning: UntypedTerm)
         (body: UntypedTerm): UntypedTerm =
           ULet(v.name, meaning, body)
}
