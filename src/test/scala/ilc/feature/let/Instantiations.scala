package ilc
package feature
package let

trait ShowTerms {
  this: Pretty =>

  def show(t: Term) =
    "\n" + pretty(t)

  def verboseShowTerm(t: Term, qual: String) =
    println(
      s"""|Raw $qual term: ${t}
          |Pretty $qual term:
          |${pretty(t)}
          |Type: ${t.getType}
          |""".stripMargin)

}

trait Instantiations {
  def buildBacchusWithLetSystem(doCSE_ : Boolean, copyPropagation_ : Boolean, partialApplicationsAreSpecial_ : Boolean) =
    new language.LetLanguage with let.ANormalFormAdapter with inference.InferenceTestHelper {
        outer =>
        val aNormalizer = new ANormalFormStateful {
          val mySyntax: outer.type = outer
          override val doCSE = doCSE_
          override val copyPropagation = copyPropagation_
          override val partialApplicationsAreSpecial = partialApplicationsAreSpecial_
        }
    }

  def buildBaseBacchus() = new language.LetLanguage

  /**
    * This will produce an AddCaches2 component which shares the component type with its argument.
    * The method type is inferred to have a dependent type: (bacchus: T): AddCaches { val mySyntax: bacchus.type }.
    * This is why you can call this method without annotating T to be a singleton type.
    */
  def buildCacher(bacchus: Syntax with IsAtomic with products.SyntaxSugar with unit.Syntax with Traversals) =
    new AddCaches2 {
      val mySyntax: bacchus.type = bacchus
    }
}

object WorksheetHelpers extends Instantiations {
  val bacchus = buildBaseBacchus()
  val cacher = buildCacher(bacchus)

  //Assert the equality of these two types as a test.
  implicitly[bacchus.Term =:= cacher.mySyntax.Term]
}
