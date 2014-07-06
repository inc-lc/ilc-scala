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
  protected val mySyntax: Syntax with IsAtomic
  type MySyntax = mySyntax.type
  import mySyntax._

  private val freshGen = new FreshGen { lazy val syntax: mySyntax.type = mySyntax }
  import freshGen._

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
  val mySyntax: Syntax with IsAtomic with Traversals
  type MySyntax = mySyntax.type
  import mySyntax._

  protected val freshGen = new FreshGen {
    //This must be lazy because at this time mySyntax is not initialized yet.
    lazy val syntax: mySyntax.type = mySyntax
  }
  import freshGen._

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
    if (isAtomic(normalT) && (copyPropagation || boundVarOpt.isEmpty)) {
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
  * That's much more confused in my head and in the code.
  * One reason is that they deal only with the first-order case.
  * Other interesting differences between what I try to do and their alg. (in Fig. 3):
  * - we encode tuples as right-nested pairs, so their rst becomes snd.
  */
//XXX: the sharing with ANormalFormStateful gives rise to a lot of non-sensical names.
//The sharing is probably still a good idea, but requires quite some refactoring to avoid this problem.
//TODO: eta-expand primitives
//TODO2: generate untyped terms, even though this'll mean duplicating code from ANormalFormStateful.
trait AddCaches extends ANormalFormStateful {
  override val mySyntax: Syntax with IsAtomic with products.SyntaxSugar with unit.Syntax with Traversals
  import mySyntax._
  import freshGen._

  //This is not supported yet for this analysis — I think this would require a concept of arity to be type-safe.
  override val partialApplicationsAreSpecial = false

  //Adapter...
  def addCaches(t: Term): Term = aNormalizeTerm(t)

  class Renames(var substs: immutable.Map[Var, Var] = immutable.Map.empty)

  override def aNormalizeTerm(t: Term, substs: immutable.Map[Var, Term]): Term = {
    implicit val bindings = createBindings(substs)
    implicit val renames = new Renames()
    //XXX We need to also use adaptCallers.
    //And we need to use the full tuples when returning, and their first component in the rest of the computation.
    val normalT = aNormalize(t)
    val withAdaptedCallers = everywhere(adaptCallers)(normalT)
    def go(s: List[(Term, Var)], t: Term, vars: List[Var]): Term =
      s match {
        case Nil =>
          //This happens to return the result twice, if it is bound to a variable before return (that is, if it's not an atomic expression).
          //XXX Figure out the expected behavior and ensure it happens.
          //Answer: if the result is worth caching, it should indeed appear twice, as visible in the case for
          //user-defined functions in Fig. 3. They're assuming that results of primitives
          //needn't be cached and should be recomputed, which isn't the case for us.
          //XXX: HOWEVER! A-normal form allows an application as a result value, which isn't good for us.
          //But that's now FIXED.
          def rename(v: Var) = (renames.substs get v) getOrElse v
          val transformedT = everywhere {
            orIdentity {
              case v: Var =>
                //transformVar(v)
                rename(v)
            }
          }(t)
          val rest =
            if (vars.isEmpty)
              UnitTerm :: Nil
            else
              vars map rename
//              transformVar//
          ((transformedT :: rest) foldLeft (tuple(rest.length + 1))) {
            _ ! _
          }
        case (term, variable) :: rest =>
          adaptCallers(renames)(Let(variable, term, go(rest, t, variable :: vars)))
      }
    go(bindings.bindings.toList,
        withAdaptedCallers,
        //normalT,
        Nil)
  }
  def transformVar(v: Var): Var = ???
  def transformVar(varName: Name, fstT: Type): Var = ???

  //XXX: what about primitives? In the paper, you can distinguish them
  //statically, here you typically can't.
  def adaptCallers(implicit renames: Renames): Term => Term =
    orIdentity {
      //XXX Probably the type is not adapted yet, so we don't match against the ProductType
      case Let(variable @ Var(varName, fstT /*ProductType(fstT, sndT)*/), App(fun, arg), body) =>
        //XXX
        case class UnknownType() extends Type
        //No freshening for now. We'll need to thread more state for that.
        val varTot = /*fresh*/(transformVar(varName, ProductType(fstT, UnknownType())))
        renames.substs += variable -> varTot
        Let(varTot, App(fun, arg),
          Let(variable, project(1) ! varTot, body))
    }
}

trait AddCaches2 {
  outer =>

  val mySyntax: Syntax with IsAtomic with products.SyntaxSugar with unit.Syntax with Traversals
  val aNormalizer = new ANormalFormStateful {
    lazy val mySyntax: outer.mySyntax.type = outer.mySyntax
    override val partialApplicationsAreSpecial = false
  }
  protected val freshGen = new FreshGen {
    //This must be lazy because at this time mySyntax is not initialized yet.
    lazy val syntax: mySyntax.type = mySyntax
  }
  import mySyntax._
  import aNormalizer._
  import freshGen._

  def addCaches: Term => Term =
    etaExpandPrimitives andThen aNormalizeTerm andThen extendReturns

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
   * Thanks to etaExpandPrimitives, primitives are fully eta-expanded, so non-nullary ones can only appear inside lambdas.
   * This means that the operator of an application can't be a primitive - except in those eta-expansion.
   * Instead of
   * app ::= atom atom
   * we have
   * app ::= name atom
   */

  //XXX also prove that the resulting term is (at least dynamically) type-safe.
  //We should even manage to get static type-safety still easily.
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
      Abs(v, descendExp(body))
  }

  def transformVar(varName: Name, fstT: Type): Var =
    Var(transformName{ name =>
      if (name endsWith "Tot") {
        //XXX AAARGH
        throw new Exception()//.printStackTrace()
        //name
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
          val varTot = fresh(transformVar(v.getName, ProductType(v.getType, UnknownType())))
          //Is this application still type-safe?
          //fun's return is correctly tupled, and its argument is not tupled
          //because it's bound by the output of descendLetRule
          Let(varTot, App(fun, arg),
            Let(v, Proj1 ! varTot, transformedBody(varTot)))

        case _: App => exp
        case _: Abs =>
          Let(v, descendAbsRule(exp), transformedBody(v))
      }
  }

  def tupleVars(intermediateResults: List[Var]): Term =?>: Term = {
    case t =>
      //XXX: Liu does use unary tuples instead! Hmm... well, in fact using unit is an encoding of them
      //with nested pairs... right? No, with nested pairs the base case is the pair of the last two vars.
      //That's one reason why project has to be so complicated...
      val rest =
        if (intermediateResults.isEmpty)
          UnitTerm :: Nil
        else
          intermediateResults
      (t :: intermediateResults foldLeft (tuple(intermediateResults.length + 1))) {
        _ ! _
      }
  }
}

object Bug {
  type =?>:[A, B] = PartialFunction[A, B]

  val ??? : Nothing = throw new Exception
  //Does not work.
  def or[T, U](f: T =?>: U)(g: T => U): T => U =
  //Works.
  //def or[T, U](f: PartialFunction[T, U])(g: T => U): T => U =
    //x => f applyOrElse (x, g)
    ???

  def bar: String =?>: String = ???
  def foo: String => String = ???
  def bippy: String => String =
    or(bar orElse bar)(foo)
}
