package ilc
package feature
package inference

import scala.language.implicitConversions

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
  def let(v: Name, meaning: UntypedTerm)
         (body: UntypedTerm): UntypedTerm =
    (v ->: body)(meaning)

  def letS(pairs: (Name, UntypedTerm)*)
          (body: UntypedTerm): UntypedTerm = {
    pairs.foldRight(body){ (pair, body) =>
      let(pair._1, pair._2)(body)
    }
  }

  //For use within letS.
  //Example:
  //letS('a := 1, 'b := 2){3}
  implicit class NameBindingOps(s: Name) {
    def :=(t: UntypedTerm): (Name, UntypedTerm) = s -> t
  }
  implicit def symToNameBindingOps(s: Symbol) = (s: Name): NameBindingOps


  //XXX Should use a macro version of const_.
  def ifThenElse_(condition: UntypedTerm, thenBranch: UntypedTerm, elseBranch: UntypedTerm) =
    IfThenElse(condition, const_(thenBranch), const_(elseBranch))

  //"Libraries"
  val const_ : UntypedTerm = 'a ->: 'b ->: 'a
}

trait LetSyntaxSugar extends SyntaxSugar with LetUntypedSyntax {
  override def let(v: Name, meaning: UntypedTerm)
         (body: UntypedTerm): UntypedTerm =
    ULet(v, meaning, body)
}
