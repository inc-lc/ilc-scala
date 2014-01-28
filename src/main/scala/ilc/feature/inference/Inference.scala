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
    case UAbs(UVar(name), body) =>
      val alpha = freshTypeVariable()
      val x = TVar(name, alpha)
      val (typedBody, c) = collectConstraints(body, extend(context, name, alpha))
      (TAbs(x, typedBody), c)
    case UApp(t1, t2) =>
      val (tt1, c1) = collectConstraints(t1, context)
      val (tt2, c2) = collectConstraints(t2, context)
      val x = freshTypeVariable()
      val c = c1 ++ c2 + Constraint(tt1.getType, Arrow(tt2.getType, x))
      (TApp(tt1, tt2, x), c)
    case _ => sys error s"Cannot infer type for $term"
  }

  def occurs(variable: TypeVariable, value: InferredType): Boolean = value match {
    case tv: TypeVariable => tv == variable
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
    def typeVariableAndAnythingElse(tn: TypeVariable, a: InferredType, remaining: Set[Constraint], substitutions: Map[TypeVariable, InferredType]) = {
      val nextRemaining = remaining.tail
      val nextSubstitutions = substitutions + ((tn, a))
      unificationHelper(substitute(nextRemaining, nextSubstitutions), nextSubstitutions)
    }
    @tailrec
    def unificationHelper(remaining: Set[Constraint], substitutions: Map[TypeVariable, InferredType]): Map[TypeVariable, InferredType] = {
      remaining.headOption match {
        case None => substitutions
        case Some((a, b)) if a == b => unificationHelper(remaining.tail, substitutions)
        case Some((tn@TypeVariable(n), a)) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
        case Some((a, tn@TypeVariable(n))) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
        case Some((Arrow(t1, t2), Arrow(t3, t4))) => unificationHelper(remaining.tail + ((t1, t3)) + ((t2, t4)), substitutions)
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

object Foo extends scala.App with ilc.feature.inference.Inference {
  val id: UntypedTerm = UAbs(UVar("x"), UVar("x"))
  printf(collectConstraints(UApp(id, id), List()).toString())
  printf(unification(collectConstraints(UApp(id, id), List())._2).toString())
//  val idType: InferredType = Arrow(TypeVariable(1), TypeVariable(1));
}

//Foo.doStuff
