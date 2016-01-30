package ilc
package feature
package let

import scalaz._
import scala.collection.generic.Growable
import collection.{immutable, mutable}

trait ANormalFormInterface {
  type MySyntax <: Syntax
  def aNormalizeTerm(t: MySyntax#Term): MySyntax#Term
}

// Delegates ANormalFormInterface to a nested instance of it.
trait ANormalFormAdapter extends ANormalFormInterface {
  outer: Syntax =>

  type MySyntax = this.type
  val aNormalizer: ANormalFormInterface { type MySyntax = outer.MySyntax }
  def aNormalizeTerm(t: Term): Term = aNormalizer.aNormalizeTerm(t)
}

/**
 * Implementation of A-normalization, based on http://matt.might.net/articles/a-normalization/.
 * Written in CPS, so stack usage might be a problem. Should that ever happen, trampolining is an alternative.
 * Alternatively, one could implement this using a writer monad, as suggested by Tillmann.
 */
trait ANormalForm extends ANormalFormInterface {
  outer =>

  protected val syntax: Syntax with IsAtomic
  type MySyntax = syntax.type
  import syntax._

  private val freshGen = new base.FreshGen { lazy val syntax: outer.syntax.type = outer.syntax }
  import freshGen.fresh

  override def aNormalizeTerm(t: Term): Term = aNormalize(t)(identity)
  def aNormalize(t: Term)(k: Term => Term): Term = t match {
    case Abs(v, body) =>
      k(Abs(v, aNormalizeTerm(body)))
    case App(operator, operand) =>
      aNormalizeName(operator) { s =>
        aNormalizeName(operand) { t =>
          k(App(s, t))
        }
      }
    case Let(variable, exp, body) =>
      aNormalize(exp) { normalExp =>
        Let(variable, normalExp, aNormalize(body)(k))
      }
    case _ if isAtomic(t) => k(t)
  }

  def aNormalizeName(t: Term)(k: Term => Term) = {
    aNormalize(t) { normalT =>
      if (isAtomic(normalT))
        k(normalT)
      else {
        val newV = fresh("a", normalT.getType)
        Let(newV, normalT, k(newV))
      }
    }
  }
}

//
trait ANormalFormStateful extends ANormalFormInterface {
  outer =>

  val syntax: Syntax with IsAtomic with Traversals
  type MySyntax = syntax.type
  import syntax._

  protected val freshGen = new base.FreshGen {
    //This must be lazy because at this time outer.syntax is not initialized yet.
    lazy val syntax: outer.syntax.type = outer.syntax
  }
  import freshGen.fresh

  abstract class Bindings(var substs: immutable.Map[Var, Term]) {
    val bindings: mutable.Traversable[(Term, Var)] with Growable[(Term, Var)]
    def += (p: (Term, Var)): Unit = {
      bindings += p
    }
    def lookup(t: Term): Option[Var]
    def isCSE: Boolean
  }
  class CSEBindings(substs: immutable.Map[Var, Term]) extends Bindings(substs) {
    //Stores all bindings in order & prevent duplicates
    override val bindings = mutable.LinkedHashMap.empty[Term, Var]
    //Reuse bindings if needed.
    def lookup(t: Term): Option[Var] = bindings get t
    def isCSE = true
  }
  class NonCSEBindings(substs: immutable.Map[Var, Term]) extends Bindings(substs) {
    //Stores all bindings in order & keep duplicates.
    override val bindings = mutable.ListBuffer.empty[(Term, Var)]
    //Never reuse an existing binding.
    def lookup(t: Term): Option[Var] = None
    def isCSE = false
  }
  /**
   * Enable common-subexpression elimination (CSE).
   */
  val doCSE = true
  /**
   * Enable copy propagation: avoid binding a variable to a new variable.
   */
  val copyPropagation = true
  val partialApplicationsAreSpecial = true

  def createBindings(substs: immutable.Map[Var, Term]) = if (doCSE) new CSEBindings(substs) else new NonCSEBindings(substs)

  //Invoked at the top-level and for each lambda body.
  //That is, for each new scope.
  /**
    * Performs a (variant of) A-normalization of the term.
    * Usually, the goal is that all intermediate results are bound to variables.
    * Here, we do not admit an application even as the final result.
    *
    * This function will rewrite the term into an equivalent* one in this A-normal
    * form, as specified by the following grammar:
    *
    * exp ::= atom | let var = appOrAbs in exp | abs
    * app ::= atom atom
    * app ::= app atom     [if partialApplicationsAreSpecial]
    * abs ::= lambda name . exp
    * appOrAbs ::= app | abs
    * atom ::= name | primitive
    *
    * If doCSE is enabled, this will also perform common-subexpression elimination
    * (CSE), that is, reuse bindings producing the same result.
    *
    * *This equivalence is stronger than beta-equivalence: operations are
    * performed in the same order.
    */
  //XXX this should be apply, and the comment should be a class comment.
  override def aNormalizeTerm(t: Term) = aNormalizeTerm(t, immutable.Map.empty)
  def aNormalizeTerm(t: Term, substs: immutable.Map[Var, Term]): Term = {
    implicit val bindings = createBindings(substs)
    val normalT = aNormalize(t)
    bindings.bindings.foldRight(normalT) {
      case ((term, variable), t) =>
        Let(variable, term, t)
    }
  }

  /*
   * The only mutable state we use are the (global) fresh variable generator,
   * and the mutable maps threaded through as parameters, initialized by calls
   * to aNormalizeTerm (at the top-level and inside each lambda).
   */
  def aNormalize(t: Term)(implicit bindings: Bindings): Term = aNormalizeTransform(bindings)(t)

  def substRule(implicit bindings: Bindings): Term =?>: Term = {
    case v: Var =>
      (bindings.substs get v) getOrElse v
  }

  def preserveAtomicRule: Term =?>: Term = {
    case t if isAtomic(t) => t
  }

  def aNormalizeMainCases(implicit bindings: Bindings): Term =?>: Term = {
    case Abs(v, body) =>
      Abs(v, aNormalizeTerm(body, bindings.substs))
    case App(operator, operand) =>
      def collectApps(t: Term, acc: List[Term]): List[Term] = t match {
        case App(s, t) => collectApps(s, t :: acc)
        case _ => t :: acc
      }
      //(Conditional) special handling for nested applications: Don't save an intermediate result for each intermediate node.
      //Note that this is very syntactic, and that's good: if the user already inserted a binding for a partial application, as in:
      // val r1 = f arg1 arg2
      // val r2 = r1 arg3
      //we don't want to inline r1 into r2, because that might lead to work duplication.
      //Here, we assume that the user inserted all needed sharing already.
      val operands =
        if (partialApplicationsAreSpecial)
          collectApps(operator, operand :: Nil)
        else
          operator :: operand :: Nil
      //Can't call aNormalizeName since it first calls aNormalize.
      aNormalizeName2(operands map (aNormalizeName(_)(bindings)) reduceLeft (App))(bindings)
    case Let(variable, exp, body) =>
      val normalExp = aNormalizeName(exp, Some(variable))
      aNormalize(body)
  }

  def aNormalizeTransform(implicit bindings: Bindings): Term => Term = aNormalizeMainCases orElse substRule orElse preserveAtomicRule

  def aNormalizeName(t: Term, boundVarOpt: Option[Var] = None)(implicit bindings: Bindings): Term = {
    aNormalizeName2(aNormalize(t), boundVarOpt)
  }

  //For overriding.
  def isAFormAtom(t: Term) = isAtomic(t)

  def aNormalizeName2(normalT: Term, boundVarOpt: Option[Var] = None)(implicit bindings: Bindings): Term = {
    def bind(): Var = {
      val newV = boundVarOpt getOrElse fresh("a", normalT.getType)
      bindings += normalT -> newV
      newV
    }
    def reuse(existingTerm: Term): Term = {
      boundVarOpt foreach { boundVar =>
        //We can't just drop an existing binding, we need to record a substitution.

        //This implements copy propagation.
        assert(isAtomic(normalT) && copyPropagation || doCSE) //Validate non-local reasoning: we only get here
        //if copyPropagation is enabled, because of the if below, or if doCSE is enabled
        bindings.substs += boundVar -> existingTerm
      }
      existingTerm
    }
    if (isAFormAtom(normalT) && (copyPropagation || boundVarOpt.isEmpty)) {
      reuse(normalT)
    } else {
      import std.option.optionSyntax._
      //Prevents calling bind() for a new binding of the same body.
      bindings lookup normalT some (reuse) none (bind())
    }
  }
}

/**
  * Implement, in essence, half of phase 1 of "Caching intermediate results for program improvement",
  * (Liu and Teitelbaum, PEPM 1995) for our lambda-calculus extended with at least
  * Let and pairs. Basically, this simply means returning all intermediate results.
  *
  * The other half is adapting every function call to cope with this API change: the caller needs to extract the first component of the return value.
  * One reason is that they deal only with the first-order case.
  * Other interesting differences between what I try to do and their alg. (in Fig. 3):
  * - we encode tuples as right-nested pairs, so their rst becomes snd.
  */
//TODO: generate untyped terms
trait AddCaches2 {
  outer =>

  val syntax: Syntax with IsAtomic with products.SyntaxSugar with unit.Syntax with Traversals
  import syntax._

  val aNormalizer = new ANormalFormStateful {
    val syntax: outer.syntax.type = outer.syntax
    override val partialApplicationsAreSpecial = false
    override def isAFormAtom(t: Term) = {
      super.isAFormAtom(t) && (t match {
        case _: Var => true
        case prim =>
          prim.getType match {
            //Need eta-expansion, so save it into its own var to ease that.
            case _ =>: _ => false
            case _ => true
          }
      })
    }
  }
  protected val freshGen = new base.FreshGen {
    //This must be lazy because at this time outer.syntax is not initialized yet.
    lazy val syntax: outer.syntax.type = outer.syntax
  }
  import aNormalizer._
  import freshGen.fresh

  def addCaches: Term => Term =
    t => extendReturns(aNormalizeTerm(t))

  def etaExpandPrimitives: Term => Term = everywhere { orIdentity {
    case v: Var => v
    case primitive if isAtomic(primitive) =>
      @annotation.tailrec def getArgsRev(typ: Type, acc: List[Type] = Nil): List[Type] = typ match {
        case s =>: t =>
          getArgsRev(t, s :: acc)
        case _ => acc
      }
      val vars = getArgsRev(primitive.getType).reverse map (fresh("eta", _))
      vars.foldRight(vars.foldLeft(primitive)(App))(Abs)
  }}

  def extendReturnsEtaExpandedPrim: Term => Term = {
    case Abs(v, body) =>
      Abs(v, Pair ! extendReturnsEtaExpandedPrim(body) ! UnitTerm)
    case t =>
      Pair ! t ! UnitTerm
  }

  //This needs to be invoked for each abs node, to reset the list of intermediate results for that scope.
  //That's similar to A-normalization.
  def extendReturns: Term => Term = descendAbsLetAtom(Nil)

  /* We implement an abstract machine traversing a structure specified by this grammar
   * (courtesy of A-normalization):
   *
   * exp ::= atom | let var = appOrAbs in exp | abs
   * app ::= atom atom
   * app ::= app atom     [if partialApplicationsAreSpecial]
   * abs ::= lambda name . exp
   * appOrAbs ::= app | abs
   * atom ::= name | primitive
   *
   * In fact, since we override isAFormAtom, this becomes:
   *
   * exp ::= atom | let var = appOrAbsOrPrim in exp | abs
   * app ::= atom atom
   * abs ::= lambda name . exp
   * appOrAbsOrPrim ::= app | abs | primitive
   * atom ::= name
   */

  //XXX also prove that the resulting term is (at least dynamically) type-safe.
  //We should even manage to get static type-safety somewhat easily.
  //The complicated thing to type is that a function and its derivative share
  //the type of "cache", but this is not yet visible here, and that's only needed
  //when we want to pack different functions together...
  //NO: consider f g, f h, where g and h are different functions with the same
  //type. They'll get different types now! But this requires only let-polymorphism.

  //Resulting term produces a tuple.
  //Proof: by invariant on descendAbsLetAtom
  def descendExp = descendAbsLetAtom(Nil)
  //Resulting term produces a tuple.
  //Proof: by inspection of tupleVars.
  def descendAtom(intermediateResults: List[Var]) = { (t: Term) =>
    assert(isAtomic(t))
    tupleVars(intermediateResults)(t)
  }

  //Resulting term produces a tuple.
  //Proof: by invariant on descendAbsRule and descendLetRule and descendAtom
  def descendAbsLetAtom(intermediateResults: List[Var]): Term => Term =
    or(descendAbsRule orElse descendLetRule(intermediateResults))(descendAtom(intermediateResults))

  //Resulting term produces a tuple.
  //Proof: by induction & invariant on descendExp
  def descendAbsRule: Term =?>: Term = {
    case Abs(v, body) =>
      val transformedBody = descendExp(body)
      Abs(v,
          if (body.isInstanceOf[Abs])
            Pair ! transformedBody ! UnitTerm
          else
            transformedBody)
  }

  def transformVar(varName: Name, fstT: Type): Var =
    Var(transformName{ name =>
      if (name endsWith "Tot") {
        throw new Exception()
      } else {
        name + "Tot"
      }
    }(varName), fstT)
  def transformVar(v: Var): Var = transformVar(v.getName, v.getType)

  //Resulting term produces a tuple.
  //Proof: by induction on descendAbsLetAtom, which is always called through
  //transformedBody.
  def descendLetRule(intermediateResults: List[Var]): Term =?>: Term = {
    case Let(v, exp, body) =>
      def transformedBody(newVar: Var) = descendAbsLetAtom(newVar :: intermediateResults)(body)
      exp match {
        case App(fun: Var, arg) if isAtomic(arg) =>
          case class UnknownType() extends Type
          //XXX This call to fresh makes the output harder to read, disabled.
          val varTot = /*fresh*/(transformVar(v.getName, ProductType(v.getType, UnknownType())))
          //Is this application still type-safe?
          //fun's return is correctly tupled, and its argument is not tupled
          //because it's bound by the output of descendLetRule
          Let(varTot, App(fun, arg),
            Let(v, Proj1 ! varTot, transformedBody(varTot)))

        case _: Abs =>
          Let(v, descendAbsRule(exp), transformedBody(v))
        case _: Var =>
          throw new RuntimeException("Should not happen!")
        case prim if isAtomic(prim) =>
          Let(v, extendReturnsEtaExpandedPrim(etaExpandPrimitives(prim)), transformedBody(v))
      }
  }

  def tupleVars(intermediateResults: List[Var]): Term =?>: Term = {
    case t =>
      //XXX: Liu does use unary tuples instead! Hmm... well, in fact using unit is an encoding of them
      //with nested pairs... right? No, with nested pairs the base case is the pair of the last two vars.
      //That's one reason why project has to be so complicated...
      val toTuple = t ::
        (if (intermediateResults.isEmpty)
          UnitTerm :: Nil
        else
          intermediateResults)
      (toTuple foldLeft (tuple(toTuple.length))) {
        _ ! _
      }
  }
}
