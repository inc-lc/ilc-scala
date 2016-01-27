package ilc
package feature
package inference

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.collection.immutable.ListMap

/* Largely inspired by http://lampwww.epfl.ch/teaching/archive/type_systems/2010/exercises/5-inference/ */

trait Inference
    extends base.Syntax
    with functions.Syntax
    with UntypedSyntax
    with Reflection {
  case class UnificationFailureDetails(unsat: Constraint, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) {
    override def toString = s"failed constraint: ${unsat.pretty()}"
    //s"remaining constraints: ${remaining.mkString("\n")}\n\nsubstitutions: ${substitutions.mkString("\n")}"
  }
  class UnificationFailure(val details: UnificationFailureDetails) extends Exception("No unification possible")
  def UnificationFailure(unsat: Constraint, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) =
    new UnificationFailure(UnificationFailureDetails(unsat, remaining, substitutions))

  // Only use this for pattern matching. Create new TypeVariables with freshTypeVariable.
  case class TypeVariable(name: Int, uterm: Option[UntypedTerm] = None) extends Type {
    // We don't want to hash uterm for every lookup.
    override def hashCode() = name
    override def toString = s"T$name"
  }

  val typeVariableCounter: AtomicInteger = new AtomicInteger()
  def _freshTypeVariable(uterm: Option[UntypedTerm]): TypeVariable = TypeVariable(typeVariableCounter.incrementAndGet(), uterm)
  def freshTypeVariable(uterm: UntypedTerm): TypeVariable = _freshTypeVariable(Some(uterm))
  def freshTypeVariable: TypeVariable = _freshTypeVariable(None)

  case class Constraint(actual: Type, expected: Type, ctx: String = "", parent: Option[Constraint] = None) {
    def pretty(showTerm: Boolean = true): String =
      s"""|Actual: ${actual}
          |Expected: ${expected}
          |${if (showTerm) s"From context: $ctx" else ""}
          |From constraint stack:
          |${parent.fold("")(_.pretty(false))}
          |""".stripMargin
  }

  def emptyConstraintSet = Set[Constraint]()

  type InferenceContext = ListMap[Name, Type]
  def lookup(context: InferenceContext, name: Name): Option[Type] =
    context get name

  def extend(context: InferenceContext, name: Name, typ: Type): InferenceContext =
    context + ((name, typ))
  def InferenceContext(l: (Name, Type)*): InferenceContext = ListMap(l: _*)

  def initVars: List[Var] = Nil

  lazy val initContext = InferenceContext(initVars map {
    case Var(name, typ) => (name, typ)
  }: _*)

  val emptyContext: InferenceContext = ListMap.empty

  sealed trait TypedTerm extends Product {
    def getType: Type
  }
  case class TVar(name: Name, typ: Type) extends TypedTerm {
    override def getType = typ
  }
  case class TAbs(argumentName: Name, argumentType: Type, body: TypedTerm) extends TypedTerm {
    override def getType = argumentType =>: body.getType
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
  case class TLet(variable: Name, varType: Type, exp: TypedTerm, body: TypedTerm) extends TypedTerm {
    override def getType = body.getType
  }
  case class TBinding(variable: Name, varType: Type, exp: TypedTerm)
  case class TLetRec(bindings: List[TBinding], bodyName: Name, body: TypedTerm) extends TypedTerm {
    override def getType = body.getType
  }

  //import shapeless._

  //This line must be after all case classes subtypes of TypedTerm, because it uses macros, so all expectations for equational reasoning are lost.
  //implicit def GenericTypedTerm = Generic[TypedTerm]

  def collectConstraints(term: UntypedTerm): (TypedTerm, Set[Constraint]) = {
    //XXX
    val (tt, c, ctx) = doCollectConstraints(term)
    val (_, newConstraints) = mergeInferenceContexts(ctx, initContext)
    (tt, c ++ newConstraints)
  }

  //This is "commutative" if we quotient types by the equations we produce,
  //has emptyContext as unit, and should probably be "associative" (types don't match,
  //but we just need to use the Set[Constraint] output with their monoid to fix this).
  def mergeInferenceContexts(ctx1: InferenceContext, ctx2: InferenceContext): (InferenceContext, Set[Constraint]) = {
    val commonNames = ctx1.keySet intersect ctx2.keySet
    val cSet = commonNames map (name => Constraint(ctx1(name), (ctx2)(name)))
    val ctx = (ctx1 -- commonNames) ++ (ctx2 -- commonNames) ++
      //we could pick ctx2, but we output equality constraints such that the
      //two choices are equivalent.
      (ctx1 filterKeys commonNames)
    (ctx, cSet)
  }

  def doCollectConstraints(term: UntypedTerm): (TypedTerm, Set[Constraint], InferenceContext) = term match {
    case UVar(name) =>
      val tVar = freshTypeVariable(term)
      (TVar(name, tVar), emptyConstraintSet, InferenceContext((name, tVar)))
    case UAbs(argumentName, annotatedArgumentType, body) =>
      val (typedBody, c, ctx) = doCollectConstraints(body)
      val argumentType = lookup(ctx, argumentName) getOrElse freshTypeVariable(term)

      val newConstraints = annotatedArgumentType.toList map (typ => Constraint(argumentType, typ))
      (TAbs(argumentName, argumentType, typedBody), c ++ newConstraints, ctx - argumentName)
    case UApp(t1, t2) =>
      val (tt1, c1, ctx1) = doCollectConstraints(t1)
      val (tt2, c2, ctx2) = doCollectConstraints(t2)
      val x = freshTypeVariable(term)
      val (ctxComb, cComb) = mergeInferenceContexts(ctx1, ctx2)
      val c = c1 ++ c2 ++ cComb + Constraint(tt1.getType, tt2.getType =>: x, s"applying $t1 to $t2")
      (TApp(tt1, tt2, x), c, ctxComb)
    case UMonomorphicConstant(term) =>
      (TMonomorphicConstant(term), emptyConstraintSet, emptyContext)
    case UPolymorphicConstant(t) =>
      val typeArguments = (1 to t.typeConstructor.arity) map (_ => freshTypeVariable(term))
      val typ = t.typeConstructor(typeArguments)
      (TPolymorphicConstant(t, typ, typeArguments), emptyConstraintSet, emptyContext)
    case TypeAscription(innerTerm, typ) =>
      val (tt, c, ctx) = doCollectConstraints(innerTerm)
      (tt, c + Constraint(tt.getType, typ, term.toString()), ctx)
    case _ => sys error s"Cannot infer type for $term"
  }

  def occurs(variable: TypeVariable, value: Type): Boolean = value match {
    case tv: TypeVariable => tv == variable
    case _ =>
      value.productIterator.exists { member =>
        member match {
          case typ: Type => occurs(variable, member.asInstanceOf[Type])
          case _         => false
        }
      }
  }

  def quickTraverse(f: Type => Type)(t: Type): Type =
    f(t traverse quickTraverse(f))

  def substituteInType(substitutions: Map[TypeVariable, Type]): Type => Type =
    quickTraverse {
      case tv: TypeVariable => substitutions.getOrElse(tv, tv)
      case typ              => typ
    }

  def substituteInConstraint(substitutions: Map[TypeVariable, Type])(constraint: Constraint): Constraint =
    Constraint(substituteInType(substitutions)(constraint.actual),
      substituteInType(substitutions)(constraint.expected), constraint.ctx)

  def substitute(substitutions: Map[TypeVariable, Type], term: TypedTerm): TypedTerm = term match {
    case TVar(name, typ)                                => TVar(name, substituteInType(substitutions)(typ))
    case TAbs(argumentName, argumentType, body)         => TAbs(argumentName, substituteInType(substitutions)(argumentType), substitute(substitutions, body))
    case TApp(t1, t2, typ)                              => TApp(substitute(substitutions, t1), substitute(substitutions, t2), substituteInType(substitutions)(typ))
    case t @ TMonomorphicConstant(_)                    => t
    case TPolymorphicConstant(term, typ, typeArguments) => TPolymorphicConstant(term, substituteInType(substitutions)(typ), typeArguments map substituteInType(substitutions))
    case anythingElse                                   => sys error s"implement substitute for $anythingElse"
  }

  def unification(constraints: Set[Constraint]): Map[TypeVariable, Type] = {
    def typeVariableAndAnythingElse(tn: TypeVariable, a: Type, remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]) = {
      val nextRemaining = remaining.tail
      val nextSubstitutions = substitutions.mapValues(substituteInType(Map(tn -> a))) + (tn -> a)
      (nextRemaining map substituteInConstraint(nextSubstitutions), nextSubstitutions)
    }
    def getTypes(p: Product) = p.productIterator.asInstanceOf[Iterator[Type]].toStream
    @tailrec
    def unificationHelper(remaining: Set[Constraint], substitutions: Map[TypeVariable, Type]): Map[TypeVariable, Type] = {
      if (remaining.isEmpty)
        substitutions
      else
        remaining.head match {
          case Constraint(a, b, _, _) if a == b => unificationHelper(remaining.tail, substitutions)
          case Constraint(tn: TypeVariable, a, _, _) if !occurs(tn, a) =>
            val (nextRemaining, nextSubstitutions) = typeVariableAndAnythingElse(tn, a, remaining, substitutions)
            unificationHelper(nextRemaining, nextSubstitutions)
          case Constraint(a, tn: TypeVariable, _, _) if !occurs(tn, a) =>
            val (nextRemaining, nextSubstitutions) = typeVariableAndAnythingElse(tn, a, remaining, substitutions)
            unificationHelper(nextRemaining, nextSubstitutions)
          case c @ Constraint(a, b, ctx, _) if a.getClass == b.getClass => unificationHelper(remaining.tail ++ (getTypes(a), getTypes(b)).zipped.map(Constraint(_, _,
            ctx, Some(c))).toSet, substitutions)
          case unsat => throw UnificationFailure(unsat, remaining, substitutions)
        }
    }
    unificationHelper(constraints, Map())
  }

  def typedTermToTerm(tt: TypedTerm): Term = tt match {
    case TVar(name, typ)                                    => Var(name, typ)
    case TAbs(argumentName, argumentType, body)             => Abs(Var(argumentName, argumentType), typedTermToTerm(body))
    case TApp(t1, t2, _)                                    => App(typedTermToTerm(t1), typedTermToTerm(t2))
    case TMonomorphicConstant(term)                         => term
    case TPolymorphicConstant(constant, typ, typeArguments) => constant(typeArguments: _*)
    case anythingElse                                       => sys error s"implement typedTermToTerm for $anythingElse"
  }

  def inferType(t: UntypedTerm): TypedTerm = {
    val (typedTerm, constraints) = collectConstraints(t)
    val substitutions = unification(constraints)
    substitute(substitutions, typedTerm)
  }
}

trait LetInference extends Inference with LetUntypedSyntax with let.Syntax {
  override def doCollectConstraints(term: UntypedTerm): (TypedTerm, Set[Constraint], InferenceContext) = term match {
    case ULet(varName, exp, body) =>
      //doCollectConstraints( ... desugared node ...)
      //desugaring result: UApp(UAbs(varName, body), exp)
      val (typedExp, constraintsExp, ctxExp) = doCollectConstraints(exp)
      val (typedBody, constraintsBody, ctxBody) = doCollectConstraints(body)

      val varType = lookup(ctxBody, varName) getOrElse freshTypeVariable(term)
      val (ctxComb, cComb) = mergeInferenceContexts(ctxExp, ctxBody - varName)
      (TLet(varName, varType, typedExp, typedBody),
        constraintsExp ++ constraintsBody ++ cComb +
        Constraint(varType, typedExp.getType, term.toString()),
        ctxComb)
    case _ => super.doCollectConstraints(term)
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
  override def doCollectConstraints(term: UntypedTerm): (TypedTerm, Set[Constraint], InferenceContext) = term match {
    case ULetRec(pairs, bodyName, body) =>
      // 1. Collect constraints from definitions of ULetRec.
      val (defBindings, defConstraints, defCtxs) = (pairs map {
        //def stands for definition
        case (defName, defUntypExp) =>
          val (defTypExp, defConstraints, defCtx) = doCollectConstraints(defUntypExp)
          val defType = lookup(defCtx, defName) getOrElse freshTypeVariable(term)
          (TBinding(defName, defTypExp.getType, defTypExp),
            defConstraints +
            Constraint(defTypExp.getType, defType, s"binding $defName — $defTypExp's type should be $defType"),
            defCtx)
      }).unzip3
      val (typedBody, bodyConstraints, bodyCtx) = doCollectConstraints(body)
      val names = pairs map (_._1)
      //match bindings with constraints from body
      val bindingsCtx = InferenceContext(defBindings map {
        case TBinding(name, typ, term) => (name, typ)
      }: _*)
      val (finalCtx, finalConstraints) = (bindingsCtx :: defCtxs).foldLeft((bodyCtx, bodyConstraints ++ defConstraints.flatten)) {
        case ((ctx1, cs), ctx2) =>
          val (ctxNew, csNew) = mergeInferenceContexts(ctx1, ctx2)
          (ctxNew, cs ++ csNew)
      }
      (TLetRec(defBindings, bodyName, typedBody), finalConstraints, finalCtx -- names)

    case _ => super.doCollectConstraints(term)
  }

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
