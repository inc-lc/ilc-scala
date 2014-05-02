package ilc.feature.inference

import ilc.feature._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.language.implicitConversions

/* Largely inspired by http://lampwww.epfl.ch/teaching/archive/type_systems/2010/exercises/5-inference/ */

trait Inference
extends base.Syntax
   with functions.Syntax
   with Reflection
{
  case class UnificationFailureDetails(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) {
    override def toString = s"remaining constraints: ${remaining.mkString("\n")}\n\nsubstitutions: ${substitutions.mkString("\n")}"
  }
  class UnificationFailure(val details: UnificationFailureDetails) extends Exception("No unification possible")
  def UnificationFailure(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) = new UnificationFailure(UnificationFailureDetails(remaining, substitutions))

  trait UntypedTerm

  case class UVar(getName: String) extends UntypedTerm
  case class UAbs(argumentName: String, typeAnnotation: Option[Type], body: UntypedTerm) extends UntypedTerm
  case class UApp(operator: UntypedTerm, operand: UntypedTerm) extends UntypedTerm
  case class UMonomorphicConstant(term: Term) extends UntypedTerm
  case class UPolymorphicConstant(term: PolymorphicConstant) extends UntypedTerm
  case class TypeAscription(term: UntypedTerm, typ: Type) extends UntypedTerm

  // Only use this for pattern matching. Create new TypeVariables with freshTypeVariable.
  case class TypeVariable(name: Int, uterm: UntypedTerm) extends Type

  val typeVariableCounter: AtomicInteger = new AtomicInteger()
  def freshTypeVariable(uterm: UntypedTerm): TypeVariable = TypeVariable(typeVariableCounter.incrementAndGet(), uterm)

  type Constraint = (Type, Type)
  def Constraint(a: Type, b: Type): Constraint = (a, b)

  def emptyConstraintSet = Set[Constraint]()

  type InferenceContext = List[(String, Type)]
  def lookup(context: InferenceContext, name: String): Option[Type] =
    context.find(p => p._1 == name).map(_._2)

  def extend(context: InferenceContext, name: String, typ: Type): InferenceContext =
    (name, typ) :: context

  def emptyContext = List()

  trait TypedTerm {
    def getType: Type
  }
  case class TVar(name: String, typ: Type) extends TypedTerm {
    override def getType = typ
  }
  case class TAbs(argumentName: String, argumentType: Type, body: TypedTerm) extends TypedTerm {
    override def getType = =>:(argumentType, body.getType)
  }
  case class TApp(t1: TypedTerm, t2: TypedTerm, typ: Type) extends TypedTerm {
    override def getType = typ
  }
  case class TMonomorphicConstant(term: Term) extends TypedTerm {
    override def getType = term.getType
  }
  case class TPolymorphicConstant(term: PolymorphicConstant, typ: Type, typeArguments: Seq[Type]) extends TypedTerm {
    override def getType = typ
  }

  def collectConstraints(term: UntypedTerm): (TypedTerm, Set[Constraint]) =
    collectConstraints(term, emptyContext)

  def collectConstraints(term: UntypedTerm, context: InferenceContext): (TypedTerm, Set[Constraint]) = term match {
    case UVar(name) =>
      lookup(context, name) match {
        case Some(typ) => (TVar(name, typ), emptyConstraintSet)
        case None => sys error s"Unbound variable ${UVar(name)}"
      }
    case UAbs(argumentName, annotatedArgumentType, body) =>
      val argumentType = annotatedArgumentType.getOrElse(freshTypeVariable(term))
      val (typedBody, c) = collectConstraints(body, extend(context, argumentName, argumentType))
      (TAbs(argumentName, argumentType, typedBody), c)
    case UApp(t1, t2) =>
      val (tt1, c1) = collectConstraints(t1, context)
      val (tt2, c2) = collectConstraints(t2, context)
      val x = freshTypeVariable(term)
      val c = c1 ++ c2 + Constraint(tt1.getType, =>:(tt2.getType, x))
      (TApp(tt1, tt2, x), c)
    case UMonomorphicConstant(term) =>
      (TMonomorphicConstant(term), emptyConstraintSet)
    case UPolymorphicConstant(t) =>
      val typeArguments = (1 to t.typeConstructor.arity) map (_ => freshTypeVariable(term))
      val typ = t.typeConstructor(typeArguments)
      (TPolymorphicConstant(t, typ, typeArguments), emptyConstraintSet)
    case TypeAscription(term, typ) =>
      val (tt, c) = collectConstraints(term, context)
      (tt, c + Constraint(tt.getType, typ))
    case _ => sys error s"Cannot infer type for $term"
  }

  def occurs(variable: TypeVariable, value: Type): Boolean = value match {
    case tv: TypeVariable => tv == variable
    case _ => value.productIterator.exists(member =>
      if (member.isInstanceOf[Type])
        occurs(variable, member.asInstanceOf[Type])
      else false)
  }

  def substitute(substitutions: Map[TypeVariable, Type]): Type => Type =
    traverse {
      case tv: TypeVariable => substitutions.getOrElse(tv, tv)
      case typ => typ
    }

  def substituteInConstraint(substitutions: Map[TypeVariable, Type])(constraint: Constraint): Constraint =
    (substitute(substitutions)(constraint._1),
     substitute(substitutions)(constraint._2))

  def substitute(constraints: Set[Constraint], substitutions: Map[TypeVariable, Type]): Set[Constraint] =
    constraints.map(substituteInConstraint(substitutions))

  def substitute(term: TypedTerm, substitutions: Map[TypeVariable, Type]): TypedTerm = term match {
    case TVar(name, typ: TypeVariable) => TVar(name, substitutions.getOrElse(typ, typ))
    case TVar(name, typ) => TVar(name, typ)
    case TAbs(argumentName, argumentType, body) => TAbs(argumentName, substitute(substitutions)(argumentType), substitute(body, substitutions))
    case TApp(t1, t2, typ) => TApp(substitute(t1, substitutions), substitute(t2, substitutions), substitute(substitutions)(typ))
    case t@TMonomorphicConstant(_) => t
    case TPolymorphicConstant(term, typ, typeArguments) => TPolymorphicConstant(term, substitute(substitutions)(typ), typeArguments map substitute(substitutions))
    case anythingElse => sys error s"implement substitute for $anythingElse"
  }

  def unification(constraints: Set[Constraint]): Map[TypeVariable, Type] = {
    def typeVariableAndAnythingElse(tn: TypeVariable, a: Type, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) = {
      val nextRemaining = remaining.tail
      val nextSubstitutions = substitutions.mapValues(substitute(Map(tn -> a))) + (tn -> a)
      unificationHelper(substitute(nextRemaining, nextSubstitutions), nextSubstitutions)
    }
    @tailrec
    def unificationHelper(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]): Map[TypeVariable, Type] = {
      remaining.headOption match {
        case None => substitutions
        case Some((a, b)) if a == b => unificationHelper(remaining.tail, substitutions)
        case Some((tn: TypeVariable, a)) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
        case Some((a, tn: TypeVariable)) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
        case Some((a, b)) if a.getClass == b.getClass =>  unificationHelper(remaining.tail ++ a.productIterator.zip(b.productIterator).toSet.asInstanceOf[Set[(Type, Type)]], substitutions)
        case _ => throw UnificationFailure(remaining, substitutions)
      }
    }
    unificationHelper(constraints, Map())
  }

  def typedTermToTerm(tt: TypedTerm): Term = tt match {
    case TVar(name, typ) => Var(name, typ)
    case TAbs(argumentName, argumentType, body) => Abs(Var(argumentName, argumentType), typedTermToTerm(body))
    case TApp(t1, t2, _) => App(typedTermToTerm(t1), typedTermToTerm(t2))
    case TMonomorphicConstant(term) => term
    case TPolymorphicConstant(constant, typ, typeArguments) => constant(typeArguments:_*)
    case anythingElse => sys error s"implement typedTermToTerm for $anythingElse"
  }

  def inferType(t: UntypedTerm): TypedTerm = {
    val (typedTerm, constraints) = collectConstraints(t)
    val substitutions = unification(constraints)
    substitute(typedTerm, substitutions)
  }

  implicit def untypedTermToTerm(t: UntypedTerm) =
    typedTermToTerm(inferType(t))

  /**
   * Take a transformer and a term, and apply transformer to each subterm of term.
   * @param transformer
   */
  def mapSubtrees(transformer: Type => Type): Type => Type =
    Type => {
      val subTypes = Type.productIterator.toList map {
        //The pattern matching cannot distinguish this.Type from (something else).Type.
        //Won't be a problem as long as you don't mix different Types in the same tree.
        case subType: Type @unchecked => transformer(subType)
        case notType => notType
      }
      reflectiveCopy(Type, subTypes: _*)
    }

  /**
   * Apply transformer to a Type bottom-up: transformer is applied to each leave,
   * then the parent node is rebuilt with the transformed leaves, then the
   * transformer is applied to the newly constructed nodes, and so forth.
   * The traversal algorithm is the same as a fold.
   *
   * If you want to implement a rewrite system, this might not be enough — you
   * might need to implement fix-point iteration, if a single rule needs to be
   * applied more than once in the same position. Since in my experience most
   * rules must be applied at most once, this is left to the rules themselves.
   *
   * Beta-reduction is a typical example of a rule needing fixpoint iteration.
   */
  def traverse(transformer: Type => Type): Type => Type =
    Type =>
      transformer(mapSubtrees(traverse(transformer))(Type))
}
