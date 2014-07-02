package ilc.feature.inference

import ilc.feature._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.language.implicitConversions

/* Largely inspired by http://lampwww.epfl.ch/teaching/archive/type_systems/2010/exercises/5-inference/ */

trait Inference
extends base.Syntax
   with functions.Syntax
   with UntypedSyntax
   with Reflection
{
  case class UnificationFailureDetails(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) {
    override def toString = s"remaining constraints: ${remaining.mkString("\n")}\n\nsubstitutions: ${substitutions.mkString("\n")}"
  }
  class UnificationFailure(val details: UnificationFailureDetails) extends Exception("No unification possible")
  def UnificationFailure(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) = new UnificationFailure(UnificationFailureDetails(remaining, substitutions))

  // Only use this for pattern matching. Create new TypeVariables with freshTypeVariable.
  case class TypeVariable(name: Int, uterm: Option[UntypedTerm] = None) extends Type

  val typeVariableCounter: AtomicInteger = new AtomicInteger()
  def freshTypeVariable(uterm: UntypedTerm): TypeVariable = TypeVariable(typeVariableCounter.incrementAndGet(), Some(uterm))

  case class Constraint(_1: Type, _2: Type, term: Option[UntypedTerm] = None)

  def emptyConstraintSet = Set[Constraint]()

  type InferenceContext = List[(String, Type)]
  def lookup(context: InferenceContext, name: String): Option[Type] =
    context.find(p => p._1 == name).map(_._2)

  def extend(context: InferenceContext, name: String, typ: Type): InferenceContext =
    (name, typ) :: context

  def emptyContext = List()

  sealed trait TypedTerm extends Product {
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

  import shapeless._

  //This line must be after all case classes subtypes of TypedTerm, because it uses macros, so all expectations for equational reasoning are lost.
  //implicit def GenericTypedTerm = Generic[TypedTerm]

  def collectConstraints(term: UntypedTerm, context: InferenceContext = emptyContext): (TypedTerm, Set[Constraint]) = term match {
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
      val c = c1 ++ c2 + Constraint(tt1.getType, =>:(tt2.getType, x), Some(term))
      (TApp(tt1, tt2, x), c)
    case UMonomorphicConstant(term) =>
      (TMonomorphicConstant(term), emptyConstraintSet)
    case UPolymorphicConstant(t) =>
      val typeArguments = (1 to t.typeConstructor.arity) map (_ => freshTypeVariable(term))
      val typ = t.typeConstructor(typeArguments)
      (TPolymorphicConstant(t, typ, typeArguments), emptyConstraintSet)
    case TypeAscription(term, typ) =>
      val (tt, c) = collectConstraints(term, context)
      (tt, c + Constraint(tt.getType, typ, Some(term)))
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
    Constraint(substitute(substitutions)(constraint._1),
     substitute(substitutions)(constraint._2), constraint.term)

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
    def getTypes(p: Product) = p.productIterator.asInstanceOf[Iterator[Type]].toStream
    @tailrec
    def unificationHelper(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]): Map[TypeVariable, Type] = {
      if (remaining.isEmpty)
        substitutions
      else
        remaining.head match {
          case Constraint(a, b, _) if a == b => unificationHelper(remaining.tail, substitutions)
          case Constraint(tn: TypeVariable, a, _) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
          case Constraint(a, tn: TypeVariable, _) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
          case Constraint(a, b, term) if a.getClass == b.getClass =>  unificationHelper(remaining.tail ++ (getTypes(a), getTypes(b)).zipped.map(Constraint(_, _, term)).toSet, substitutions)
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

  def onTypes[T](transformer: Type => Type): T => T = {
    //The pattern matching cannot distinguish this.Type from (something else).Type.
    //Won't be a problem as long as you don't mix different Types in the same tree.
    case subType: Type @unchecked => transformer(subType).asInstanceOf[T]
    case notType: Product => mapSubtrees(transformer)(notType).asInstanceOf[T]
    case v: Traversable[u] => (v map onTypes(transformer)).asInstanceOf[T]
    case notProduct => notProduct
  }

  /**
   * Take a transformer and a term, and apply transformer to each subterm of term.
   * @param transformer
   */
  def mapSubtrees[T <: Product](transformer: Type => Type): T => T =
    typ => {
      val subTypes = typ.productIterator.toList map onTypes(transformer)
      reflectiveCopy(typ, subTypes: _*)
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
  def traverse[T <: Product](transformer: Type => Type): T => T =
    typ =>
      onTypes(transformer)(mapSubtrees(traverse(transformer))(typ))
}
