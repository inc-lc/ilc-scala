package ilc.feature.inference

import ilc.feature._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

trait Inference
extends base.Syntax
   with functions.Syntax
{

  trait UntypedTerm

  // NOTE: no idea how to import `Name`, but should probably use that instead
  //       Or maybe not. I forget.
  case class UVar(getName: String) extends UntypedTerm

  case class UAbs(variable: UVar, body: UntypedTerm) extends UntypedTerm

  case class UApp(operator: UntypedTerm, operand: UntypedTerm) extends UntypedTerm


  trait InferredType

  // Only use this for pattern matching. Create new TypeVariables with freshTypeVariable.
  case class TypeVariable(name: Int) extends InferredType

  val typeVariableCounter: AtomicInteger = new AtomicInteger()
  def freshTypeVariable(): TypeVariable = {
    val name = typeVariableCounter.incrementAndGet()
    TypeVariable(name)
  }

  case class Arrow(left: InferredType, right: InferredType) extends InferredType


  type Constraint = (InferredType, InferredType)
  def Constraint(a: InferredType, b: InferredType): Constraint = (a, b)

  def emptyConstraintSet = Set[Constraint]()

  type Context = List[(String, InferredType)]
  def lookup(context: Context, name: String): Option[InferredType] =
    context.find(p => p._1 == name).map(_._2)

  def extend(context: Context, name: String, typ: InferredType): Context =
    (name, typ) :: context

  trait TypedTerm {
    def getType: InferredType
  }
  case class TVar(name: String, typ: InferredType) extends TypedTerm {
    override def getType = typ
  }
  case class TAbs(variable: TVar, body: TypedTerm) extends TypedTerm {
    override def getType = Arrow(variable.getType, body.getType)
  }
  // Not so happy with this. There is a redundancy in types.
  case class TApp(t1: TypedTerm, t2: TypedTerm, typ: InferredType) extends TypedTerm {
    override def getType = typ
  }

  def collectConstraints(term: UntypedTerm, context: Context): (TypedTerm, Set[Constraint]) = term match {
    case UVar(name) =>
      lookup(context, name) match {
        case Some(typ) => (TVar(name, typ), emptyConstraintSet)
        case None => sys error s"Unbound variable ${UVar(name)}"
      }
    case UAbs(UVar(name), body) => {
      val alpha = freshTypeVariable()
      val x = TVar(name, alpha)
      val (typedBody, c) = collectConstraints(body, extend(context, name, alpha))
      (TAbs(x, typedBody), c)
    }
    case UApp(t1, t2) => {
      val (tt1, c1) = collectConstraints(t1, context)
      val (tt2, c2) = collectConstraints(t2, context)
      val x = freshTypeVariable()
      val c = c1 ++ c2 + Constraint(tt1.getType, Arrow(tt2.getType, x))
      (TApp(tt1, tt2, x), c)
    }
    case term => sys error s"Cannot infer type for $term"
  }

  def occurs(variable: TypeVariable, value: InferredType): Boolean = value match {
    // what are == and equals in Scala exactly?
    case tv: TypeVariable => tv equals variable
    case Arrow(t1, t2) => occurs(variable, t1) || occurs(variable, t2)
    case anythingElse => sys error s"implement occurs for $anythingElse"
  }

  def substituteInType(typ: InferredType, substitutions: Map[TypeVariable, InferredType]): InferredType = typ match {
    case tv@TypeVariable(n) => substitutions.getOrElse(tv, tv)
    case Arrow(t1, t2) => Arrow(substituteInType(t1, substitutions), substituteInType(t2, substitutions))
    case anythingElse => sys error s"implement substituteInType for $anythingElse"
  }

  def substituteInConstraint(substitutions: Map[TypeVariable, InferredType])(constraint: Constraint): Constraint =
    (substituteInType(constraint._1, substitutions),
     substituteInType(constraint._2, substitutions))

  def substitute(constraints: Set[Constraint], substitutions: Map[TypeVariable, InferredType]): Set[Constraint] =
    constraints.map(substituteInConstraint(substitutions))

  def unification(constraints: Set[Constraint]): Map[TypeVariable, InferredType] = {
    @tailrec
    def unificationHelper(remaining: Set[Constraint], substitutions: Map[TypeVariable, InferredType]): Map[TypeVariable, InferredType] = {
      remaining.take(1) match {
        case (a, b) if a equals b => unificationHelper(remaining.drop(1), substitutions)
        // why do I need the InferredType annotation here?
        case (tn@TypeVariable(n), a: InferredType) if !occurs(tn, a) => {
          val nextRemaining = remaining.drop(1)
          val nextSubstitutions = substitutions + ((tn, a))
          unificationHelper(substitute(nextRemaining, nextSubstitutions), substitutions)
        }
        // TODO Symmetric case
        // I would like to do just:
        // case (tn@TypeVariable(n), a) | (a, tn@TypeVariable(n)) if ... but that does not work https://issues.scala-lang.org/browse/SUGGEST-25
        case (Arrow(t1, t2), Arrow(t3, t4)) => unificationHelper(remaining.drop(1) + ((t1, t3)) + ((t2, t4)), substitutions)
        case _ => sys error "No substitution possible."
      }

    }
    unificationHelper(constraints, Map())
  }

  /* This seems somewhat reasonable so far.
     (Largely inspired by http://lampwww.epfl.ch/teaching/archive/type_systems/2010/exercises/5-inference/
      Maybe I should have looked for solutions, not exercises, but whatever...)
     Problems:
     Substitution is implemented for types. Need to do this for TypedTerms (again?)
     Largely untested
   */

}

// My workflow for the Scala REPL sucks.
// Is it possible to trick the REPL into thinking it is inside some object that extends a given trait so I can just use and evaluate stuff?

import ilc.feature.inference.Inference
object Foo extends Inference {

  def doStuff = {

    val id: UntypedTerm = UAbs(UVar("x"), UVar("x"))

    printf(collectConstraints(UApp(id, id), List()).toString())

  }
//  val idType: InferredType = Arrow(TypeVariable(1), TypeVariable(1));
}

//Foo.doStuff
