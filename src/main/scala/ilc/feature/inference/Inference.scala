package ilc.feature.inference

import ilc.feature._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

trait Inference
extends base.Syntax
   with functions.Syntax
{
  class UnificationFailure extends Exception("No unification possible")

  trait UntypedTerm

  case class UVar(getName: String) extends UntypedTerm
  case class UAbs(variable: UVar, body: UntypedTerm) extends UntypedTerm
  case class UApp(operator: UntypedTerm, operand: UntypedTerm) extends UntypedTerm
  case class UTerm(term: Term) extends UntypedTerm

  type InferredType = Type

  // Only use this for pattern matching. Create new TypeVariables with freshTypeVariable.
  case class TypeVariable(name: Int) extends InferredType

  val typeVariableCounter: AtomicInteger = new AtomicInteger()
  def freshTypeVariable(): TypeVariable = {
    val name = typeVariableCounter.incrementAndGet()
    TypeVariable(name)
  }

//  case class Arrow(left: InferredType, right: InferredType) extends InferredType
  val Arrow = =>:


  type Constraint = (InferredType, InferredType)
  def Constraint(a: InferredType, b: InferredType): Constraint = (a, b)

  def emptyConstraintSet = Set[Constraint]()

  type Context = List[(String, InferredType)]
  def lookup(context: Context, name: String): Option[InferredType] =
    context.find(p => p._1 == name).map(_._2)

  def extend(context: Context, name: String, typ: InferredType): Context =
    (name, typ) :: context

  def emptyContext = List()

  trait TypedTerm {
    def getType: InferredType
  }
  case class TVar(name: String, typ: InferredType) extends TypedTerm {
    override def getType = typ
  }
  case class TAbs(variable: TVar, body: TypedTerm) extends TypedTerm {
    override def getType = Arrow(variable.getType, body.getType)
  }
  case class TApp(t1: TypedTerm, t2: TypedTerm, typ: InferredType) extends TypedTerm {
    override def getType = typ
  }
  case class TTerm(term: Term, typ: InferredType) extends TypedTerm {
    override def getType = typ
  }

  def collectConstraints(term: UntypedTerm): (TypedTerm, Set[Constraint]) =
    collectConstraints(term, emptyContext)

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
    case _ => value.productIterator.exists(member =>
      if (member.isInstanceOf[InferredType])
        occurs(variable, member.asInstanceOf[InferredType])
      else false)
  }

  def substitute(typ: InferredType, substitutions: Map[TypeVariable, InferredType]): InferredType = typ match {
    case tv@TypeVariable(n) => substitutions.getOrElse(tv, tv)
    case Arrow(t1, t2) => Arrow(substitute(t1, substitutions), substitute(t2, substitutions))
    case anythingElse => sys error s"implement substitute for $anythingElse"
  }

  def substituteInConstraint(substitutions: Map[TypeVariable, InferredType])(constraint: Constraint): Constraint =
    (substitute(constraint._1, substitutions),
     substitute(constraint._2, substitutions))

  def substitute(constraints: Set[Constraint], substitutions: Map[TypeVariable, InferredType]): Set[Constraint] =
    constraints.map(substituteInConstraint(substitutions))

  def substitute(term: TypedTerm, substitutions: Map[TypeVariable, InferredType]): TypedTerm = term match {
    case TVar(name, typ: TypeVariable) => TVar(name, substitutions.getOrElse(typ, typ))
    // NOTE: We know (by contract) that substitution on a TVar should produce a TVar.
    // I guess it would be possible to statically guarantee this, but is it worth it?
    case TAbs(variable, body) => TAbs(substitute(variable, substitutions).asInstanceOf[TVar], substitute(body, substitutions))
    case TApp(t1, t2, typ) => TApp(substitute(t1, substitutions), substitute(t2, substitutions), substitute(typ, substitutions))
    case anythingElse => sys error s"implement substitute for $anythingElse"
  }

  def unification(constraints: Set[Constraint]): Map[TypeVariable, InferredType] = {
    def typeVariableAndAnythingElse(tn: TypeVariable, a: InferredType, remaining: Set[Constraint], substitutions: Map[TypeVariable, InferredType]) = {
      val nextRemaining = remaining.tail
      val nextSubstitutions = substitutions.mapValues(substitute(_, Map(tn -> a))) + (tn -> a)
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
        case _ => throw new UnificationFailure()
      }
    }
    unificationHelper(constraints, Map())
  }

  /* Largely inspired by http://lampwww.epfl.ch/teaching/archive/type_systems/2010/exercises/5-inference/
     Problems:
     Largely untested
   */

}
