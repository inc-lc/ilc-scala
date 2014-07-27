package ilc
package feature
package inference

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
  case class UnificationFailureDetails(unsat: Constraint, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) {
    override def toString = s"failed constraint: ${unsat.pretty()}"
      //s"remaining constraints: ${remaining.mkString("\n")}\n\nsubstitutions: ${substitutions.mkString("\n")}"
  }
  class UnificationFailure(val details: UnificationFailureDetails) extends Exception("No unification possible")
  def UnificationFailure(unsat: Constraint, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) =
    new UnificationFailure(UnificationFailureDetails(unsat, remaining, substitutions))

  // Only use this for pattern matching. Create new TypeVariables with freshTypeVariable.
  case class TypeVariable(name: Int, uterm: Option[UntypedTerm] = None) extends Type

  val typeVariableCounter: AtomicInteger = new AtomicInteger()
  def freshTypeVariable(uterm: UntypedTerm): TypeVariable = TypeVariable(typeVariableCounter.incrementAndGet(), Some(uterm))

  case class Constraint(_1: Type, _2: Type, ctx: String = "", parent: Option[Constraint] = None) {
    def pretty(showTerm: Boolean = true): String =
      s"""|Actual: ${_1}
          |Expected: ${_2}
          |${if (showTerm) s"From context: $ctx" else ""}
          |From constraint stack:
          |${parent.fold("")(_.pretty(false)) }
          |""".stripMargin
  }

  def emptyConstraintSet = Set[Constraint]()

  type InferenceContext = List[(String, Type)]
  def lookup(context: InferenceContext, name: String): Option[Type] =
    context.find(p => p._1 == name).map(_._2)

  def extend(context: InferenceContext, name: String, typ: Type): InferenceContext =
    (name, typ) :: context

  def initVars: List[Var] = Nil

  lazy val emptyContext: InferenceContext = initVars map {
    case Var(name, typ) => (name.toString, typ)
  }

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
  //XXX this should be in LetInference, but let's not unseal stuff for now, that might break shapeless.
  //However, the handling is only
  case class TLet(variable: String, varType: Type, exp: TypedTerm, body: TypedTerm) extends TypedTerm {
    override def getType = body.getType
  }
  case class TBinding(variable: String, varType: Type, exp: TypedTerm)
  case class TLetRec(bindings: List[TBinding], bodyName: String, body: TypedTerm) extends TypedTerm {
    override def getType = body.getType
  }

  //import shapeless._

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
      val c = c1 ++ c2 + Constraint(tt1.getType, =>:(tt2.getType, x), s"applying $t1 to $t2")
      (TApp(tt1, tt2, x), c)
    case UMonomorphicConstant(term) =>
      (TMonomorphicConstant(term), emptyConstraintSet)
    case UPolymorphicConstant(t) =>
      val typeArguments = (1 to t.typeConstructor.arity) map (_ => freshTypeVariable(term))
      val typ = t.typeConstructor(typeArguments)
      (TPolymorphicConstant(t, typ, typeArguments), emptyConstraintSet)
    case TypeAscription(innerTerm, typ) =>
      val (tt, c) = collectConstraints(innerTerm, context)
      (tt, c + Constraint(tt.getType, typ, term.toString()))
    case _ => sys error s"Cannot infer type for $term"
  }

  def occurs(variable: TypeVariable, value: Type): Boolean = value match {
    case tv: TypeVariable => tv == variable
    case _ => value.productIterator.exists(member =>
      if (member.isInstanceOf[Type])
        occurs(variable, member.asInstanceOf[Type])
      else false)
  }

  def substituteInType(substitutions: Map[TypeVariable, Type]): Type => Type =
    traverse {
      case tv: TypeVariable => substitutions.getOrElse(tv, tv)
      case typ => typ
    }

  def substituteInConstraint(substitutions: Map[TypeVariable, Type])(constraint: Constraint): Constraint =
    Constraint(substituteInType(substitutions)(constraint._1),
     substituteInType(substitutions)(constraint._2), constraint.ctx)

  def substitute(substitutions: Map[TypeVariable, Type], term: TypedTerm): TypedTerm = term match {
    case TVar(name, typ) => TVar(name, substituteInType(substitutions)(typ))
    case TAbs(argumentName, argumentType, body) => TAbs(argumentName, substituteInType(substitutions)(argumentType), substitute(substitutions, body))
    case TApp(t1, t2, typ) => TApp(substitute(substitutions, t1), substitute(substitutions, t2), substituteInType(substitutions)(typ))
    case t@TMonomorphicConstant(_) => t
    case TPolymorphicConstant(term, typ, typeArguments) => TPolymorphicConstant(term, substituteInType(substitutions)(typ), typeArguments map substituteInType(substitutions))
    case anythingElse => sys error s"implement substitute for $anythingElse"
  }

  def unification(constraints: Set[Constraint]): Map[TypeVariable, Type] = {
    def typeVariableAndAnythingElse(tn: TypeVariable, a: Type, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) = {
      val nextRemaining = remaining.tail
      val nextSubstitutions = substitutions.mapValues(substituteInType(Map(tn -> a))) + (tn -> a)
      unificationHelper(nextRemaining map substituteInConstraint(nextSubstitutions), nextSubstitutions)
    }
    def getTypes(p: Product) = p.productIterator.asInstanceOf[Iterator[Type]].toStream
    @tailrec
    def unificationHelper(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]): Map[TypeVariable, Type] = {
      if (remaining.isEmpty)
        substitutions
      else
        remaining.head match {
          case Constraint(a, b, _, _) if a == b => unificationHelper(remaining.tail, substitutions)
          case Constraint(tn: TypeVariable, a, _, _) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
          case Constraint(a, tn: TypeVariable, _, _) if !occurs(tn, a) => typeVariableAndAnythingElse(tn, a, remaining, substitutions)
          case c @ Constraint(a, b, ctx, _) if a.getClass == b.getClass => unificationHelper(remaining.tail ++ (getTypes(a), getTypes(b)).zipped.map(Constraint(_, _,
              ctx, Some(c))).toSet, substitutions)
          case unsat => throw UnificationFailure(unsat, remaining, substitutions)
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
    substitute(substitutions, typedTerm)
  }

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

trait LetInference extends Inference with LetUntypedSyntax with let.Syntax {
  override def collectConstraints(term: UntypedTerm, context: InferenceContext = emptyContext): (TypedTerm, Set[Constraint]) = term match {
    case ULet(variable, exp, body) =>
      //collectConstraints( ... desugared node ...)
      //desugaring result: UApp(UAbs(variable, body), exp)
      val (typedExp, c1) = collectConstraints(exp, context)
      val argType = freshTypeVariable(term)
      val (typedBody, c2) = collectConstraints(body, extend(context, variable, argType))
      val c = c1 ++ c2 + Constraint(argType, typedExp.getType, term.toString())
      (TLet(variable, argType, typedExp, typedBody), c)
    case _ => super.collectConstraints(term, context)
  }

  override def substitute(substitutions: Map[TypeVariable, Type], term: TypedTerm): TypedTerm = term match {
    case TLet(variable, varType, exp, body) =>
      TLet(variable, substituteInType(substitutions)(varType), substitute(substitutions, exp), substitute(substitutions, body))
    case _ => super.substitute(substitutions, term)
  }

  override def typedTermToTerm(tt: TypedTerm): Term = tt match {
    case TLet(variable, varType, exp, body) =>
      Let(Var(variable, varType), typedTermToTerm(exp), typedTermToTerm(body))
    case _ => super.typedTermToTerm(tt)
  }
}

trait LetRecInference extends Inference with LetRecUntypedSyntax with functions.LetRecSyntax {
  override def collectConstraints(term: UntypedTerm, context: InferenceContext = emptyContext): (TypedTerm, Set[Constraint]) = term match {
    case ULetRec(pairs, bodyName, body) =>
      val tVars: List[(String, TypeVariable)] = pairs map (_._1) map ((_, freshTypeVariable(term)))
      val extCtx = tVars.foldLeft(context) {
        (ctx, newTVar) => extend(ctx, newTVar._1, newTVar._2)
      }
      val (typedExps, constraintss) = (pairs map { case (variable, exp) => collectConstraints(exp, extCtx)}).unzip
      val (typedBody, c2) = collectConstraints(body, extCtx)
      val expMatchConstraints = typedExps zip tVars map {
        case (exp, v@(varName, varT)) => Constraint(varT, exp.getType, s"matching $exp and $v")
      }
      val c = c2 ++ constraintss.flatten ++ expMatchConstraints
      val bindings = (pairs map (_._1)) zip (tVars map (_._2)) zip typedExps map {
        case ((a, b), c) => TBinding(a, b, c)
      }
      (TLetRec(bindings, bodyName, typedBody), c)
    case _ => super.collectConstraints(term, context)
  }

  //  case class TLetRec(bindings: List[(String, Type, TypedTerm)], body: TypedTerm)

  override def substitute(substitutions: Map[TypeVariable, Type], term: TypedTerm): TypedTerm = term match {
    case TLetRec(bindings, bodyName, body) =>
      val substBindings = bindings map {
        case TBinding(variable, varType, exp) =>
          TBinding(variable, substituteInType(substitutions)(varType), substitute(substitutions, exp))
      }
      TLetRec(substBindings, bodyName, substitute(substitutions, body))
    case _ => super.substitute(substitutions, term)
  }

  override def typedTermToTerm(tt: TypedTerm): Term = tt match {
    case TLetRec(bindings, bodyName, body) =>
      val convBindings: List[(Var, Term)] = bindings map {
        case TBinding(variable, varType, exp) =>
          (Var(variable, varType), typedTermToTerm(exp))
      }
      LetRec(convBindings, bodyName, typedTermToTerm(body))
    case _ => super.typedTermToTerm(tt)
  }
}
