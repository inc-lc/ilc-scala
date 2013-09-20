package ilc
package feature
package base

import scala.language.implicitConversions
import scala.language.postfixOps

trait Syntax
extends Types
   with Names
// We need function type constructor =>: to support inference
// of type arguments of a polymorphic constant from a specification
// of how its type is to be computed.
//
// This is a restricted form of rank-1 (ML-style) polymorphism.
// in addition to having all type quantifiers at the left-most
// side of a polymorphic constant's type signature, the result
// type of the constant will not be inferred. Type variables
// occurring within the result type alone must be given by the
// user.
   with functions.Types
{
  trait Typed {
    /** Returns type or fails horribly. */
    def getType: Type
  }

  trait Term extends Typed {
    // forces early failure for object-level type checking
    if(getType == null) {
      val className = this.getClass.getName
      throw new java.lang.NullPointerException(
        s"""
$className.getType must be accessible in superclass constructor
to support early failure for object-level type checking.
Please do not declare getType as an abstract `val`.
"""
      )
    }
  }

  // Variable

  /** The supertrait of all types of variables
    */
  trait Variable extends Term {
    def getName: Name
    def getType: Type
  }

  /** The variable that a user writes:
    * x, y, z as opposed to dx, ddy, dddz
    */
  case class Var(getName: Name, getType: Type) extends Variable

  // TYPING CONTEXT

  case class TypingContext(toList: List[Variable]) {
    def lookup(name: Name): Option[Variable] =
      this.toList.find(_.getName == name)

    /** find the name, or die **/
    def apply(name: Name): Variable =
      lookup(name).fold(typeErrorNotDefined(name))(identity)

    /** membership test */
    def contains(name: Name): Boolean =
      lookup(name).fold(false)(_ => true)

    /** add a (typed) variable to the typing context */
    def +: (variable: Variable): TypingContext =
      TypingContext(variable :: this.toList)
  }

  object TypingContext {
    val empty: TypingContext = TypingContext(Nil)
    def apply(variables: Variable*): TypingContext =
      TypingContext(List(variables: _*))
  }

  /** Generate a name unbound in context
    *
    * @param context the typing context whose bound names are
    *                to be avoided
    * @param default the default name
    * @return `default` wrapped in the fewest number of
    *         `FreshName` constructors to make it outside
    *         of `context`
    *
    * {{{
    * freshName(englishMonachs, "Attila")    = "Attila"
    * freshName(englishMonachs, "Elisabeth") = "Elizabeth_3"
    * }}}
    */
  def freshName(context: TypingContext, default: Name): Name = {
    var newName = default
    var index = 0
    while (context contains newName) {
      index += 1
      newName = IndexedName(default, index)
    }
    newName
  }

  // POLYMORPHIC TERMS

  trait PolymorphicTerm {
    def toTerm: Term = specialize(List.empty: _*)

    /** Specialize a polymorphic term builder according to
      * the types of future arguments
      *
      * Implementation of this method is the sole obligation
      * of subclasses
      *
      * @param argumentTypes types of future arguments
      * @return a term builder free from type abstractions
      */
    def specialize(argumentTypes: Type*): Term

    protected[this]
    def argumentTypesMatch(argTypes: Seq[Type], funType: Type): Boolean =
      getArgumentTypes(funType) startsWith argTypes
  }

  /** PolymorphicTerm  without subterms.
    * Subclasses need only declare how its type
    * is to be computed; the actual type specialization
    * code is inherited.
    *
    * Example:
    * {{{
    * object Lookup extends PolymorphicConstant {
    *   val typeConstructor = TypeConstructor("keyType, valueType") {
    *     case Seq(keyType, valueType) =>
    *       keyType =>: MapType(keyType, valueType) =>: valueType
    *   }
    * }
    * }}}
    */
  trait PolymorphicConstant extends PolymorphicTerm {
    theConstant =>

    /** The only subclass obligation: A variadic type
      * function that computes the type of this polymorphic
      * constant given a number of type arguments
      */
    def typeConstructor: TypeConstructor

    /** Construct a term corresponding to the specialized
      * polymorphic constant
      *
      * Example:
      * {{{
      * object Lookup extends PolymorphicConstant { ... }
      *
      * val lookupTerm: Term = Lookup(keyType, valType)
      * }}}
      */
    def apply(typeArguments: Type*): Term =
      Constant(typeArguments)

    // inverting the type constructor is necessary
    // to specialize a polymorphic constant according to the
    // types of its arguments, hence the class TypeConstructor
    // below.
    def specialize(argumentTypes: Type*): Term =
      Constant(typeConstructor inferTypeArgumentsFrom argumentTypes)

    def of(expectedType: Type): Term = {
      val typeArguments = typeConstructor.inverse(expectedType)
      val term = apply(typeArguments: _*)
      require(term.getType == expectedType)
      term
    }

    /** description of a type constructor with enough
      * details at the level of scala values to compute
      * its own inverse
      *
      * Example usage:
      * {{{
      * // foldNat : r → (r → r) → ℕ → r
      * object FoldNat extends ConstantWith1TypeParameter {
      *   val typeConstructor = TypeConstructor("r") { r =>
      *     r =>: (r =>: r) =>: NatType =>: r
      *   }
      * }
      * 
      * object Lookup extends ConstantWith2TypeParameters {
      *   val typeConstructor = TypeConstructor("k", "v") {
      *     case Seq(k, v) =>
      *       k =>: MapType(k, v) =>: v
      *   }
      * }
      * }}}
      */
    protected[this]
    object TypeConstructor {
      def apply(typeVariableName: Name)
               (typeFunction: Type => Type): TypeConstructor =
        TypeConstructor(Seq(typeVariableName): _*) {
          case Seq(typeArgument) =>
            typeFunction(typeArgument)
        }
    }

    protected[this]
    case class TypeConstructor(typeVariableNames: Name*)
      (typeFunction: Seq[Type] => Type)
    {
      /** given type arguments, compute the result, which is a type */
      def apply(typeArguments: Seq[Type]): Type =
        typeFunction(typeArguments)

      /** given the result (which is a type), compute type arguments */
      def inverse(expectedType: Type): Seq[Type] = {
        val accumulator = mkAccumulator()
        matchTypes(abstractType, expectedType, accumulator)
        postProcess(accumulator)
      }

      /** given argument types, compute type arguments */
      def inferTypeArgumentsFrom(argumentTypes: Seq[Type]): Seq[Type] = {
        val accumulator = mkAccumulator()
        // `scala.Tuple2.zipped` zips two sequences together
        // such that the result is as long as the shorter one
        // of the two sequences. Here we exploit that fact to
        // support curried syntax. No type argument needs be
        // given to a primitive with enough arguments to deduce
        // its type arguments.
        // {{{
        // val t1: Term = Maybe ! 0 ! lambda(NatType) {x => x}
        // t1.getType == MaybeType(NatType) =>: NatType
        // }}}
        (abstractArgumentTypes, argumentTypes).zipped foreach {
          (abstractType, concreteType) =>
            matchTypes(abstractType, concreteType, accumulator)
        }
        postProcess(accumulator)
      }

      // TypeVariable is chosen to be integers because
      //
      // 1. they are easily guaranteed to be unique
      //
      // 2. we can exploit the invariant `typeVariables(i).index == i`
      //    to simplify inverting the type constructor
      private[this] case class TypeVariable(index: Int) extends Type
      private[this] val typeVariables: Seq[TypeVariable] =
        typeVariableNames.zipWithIndex.map {
          case (name, index) => TypeVariable(index)
        }

      private[this] val arity = typeVariables.size

      // necessary for inversion and force early failure if
      // typeFunction is specified with incorrect arity
      private[this] val abstractType =
        typeFunction(typeVariables)
      private[this] val abstractArgumentTypes =
        getArgumentTypes(abstractType)

      private[this] def mkAccumulator(): Array[List[Type]] =
        Array.fill(arity) { Nil }
      /** match abstract type against a concrete type to obtain
        * type parameters
        *
        * CAVEAT: exploits the fact that all types are case classes of
        *         other types.  We must uphold this rule for it to not
        *         throw errors at runtime
        *
        * @param abstractType type with holes between 0 and (arity - 1)
        * @param concreteType type to match against
        * @param accumulator (inout) mutable accumulator,
        *        an `Array[List[Type]]` whose `i`th element contains
        *        all the types matched against the `i`th type variable
        *        thus far
        * @return nothing of value
        */
      private[this] def matchTypes(
        abstractType: Type,
        concreteType: Type,
        accumulator: Array[List[Type]]
      ): Unit = {
        (abstractType, concreteType) match {
        case (TypeVariable(i), _) => {
          accumulator.update(i, concreteType :: accumulator(i))
        }

        case (theAbstract: Product, theConcrete: Product) => {
          // test if the type constructor are instances of the same Scala class.
          // Since case-case inheritance is forbidden, this is enough to check
          // that classOf[theConcrete] <: classOf[theAbstract].
          if (! theAbstract.getClass.isInstance(theConcrete))
            typeErrorNotTheSame(s"instantiating ${getConstantName}",
              theAbstract,
              theConcrete)
          if (theAbstract.productArity != theConcrete.productArity)
            typeErrorNotTheSame(
              s"instantiating the type of $getConstantName",
              theAbstract,
              theConcrete)
          (0 until theAbstract.productArity) foreach { i =>
            matchTypes(
              theAbstract.productElement(i).asInstanceOf[Type],
              theConcrete.productElement(i).asInstanceOf[Type],
              accumulator)
          }
        }

        case _ => {
          throw new TypeError(
            s"""|Cannot invert type constructor of $getConstantName.
                |Two possible scenarios.
                |
                |1. The abstract type has children, whereas the concrete type
                |   is at a leaf. They don't match. There is a type error with
                |   the object code.
                |
                |2. There is a type of higher kind that is not a case class.
                |   please make it a case class.
                |
                |In any case, here are the types.
                |
                |abstractType =
                |  $abstractType
                |
                |concreteType =
                |  $concreteType
                |""".stripMargin)
        }
    }
  }

      /** process all info gained about type variables, report
        * their types or fail with an error
        */
      def postProcess(typeInfo: Array[List[Type]]): Seq[Type] =
        typeInfo.zipWithIndex map {
          case (Nil, i) => {
            val typeVariableName = typeVariableNames(i)
            sys error s"type variable $typeVariableName " +
              s"unused in body of $getConstantName"
          }

          case (candidate :: otherWitnesses, i) => {
            assertTypeInfoConsistency(candidate, otherWitnesses, i)
            candidate
          }
        }

      /** checks that all accounts of a type variable are of the
        * same type */
      def assertTypeInfoConsistency(
        candidate: Type,
        otherWitnesses: List[Type],
        variableIndex: Int
      ) {
        otherWitnesses foreach { witness =>
          if (witness != candidate)
            typeErrorNotTheSame(
              "inferring the type parameter " +
                typeVariableNames(variableIndex) +
                s" of $getConstantName",
              candidate,
              witness
            )
        }
      }
    }

    /** the constant with all type parameters supplied */
    protected[this] case class Constant(typeArguments: Seq[Type])
    extends Term
    {
      def getType: Type = typeConstructor(typeArguments)

      /** mimic the generated toString method of case classes */
      override def toString: String = {
        val printedTypeArguments = typeArguments mkString ", "
        s"$getConstantName($printedTypeArguments)"
      }
    }

    protected[this] def getConstantName: String = {
        // objectName = "ilc.feature.maps.Lookup$" for example
        val objectName = theConstant.getClass.getName
        // simpleName = "Lookup"
        // we can't use java.lang.Class.getSimpleName because
        // it throws java.lang.InternalError: Malformed class name
        // at java.lang.Class.getSimpleName(Class.java:1153)
        // (java version "1.7.0_05")
        objectName split """[\.\$]""" last
    }
  }

  trait ConstantWith1TypeParameter extends PolymorphicConstant {
    def unapply(t: Term): Option[Type] = t match {
      case Constant(Seq(t1)) => Some(t1)
      case _ => None
    }
  }

  trait ConstantWith2TypeParameters extends PolymorphicConstant {
    def unapply(t: Term): Option[(Type, Type)] = t match {
      case Constant(Seq(t1, t2)) => Some((t1, t2))
      case _ => None
    }
  }

  trait ConstantWith3TypeParameters extends PolymorphicConstant {
    def unapply(t: Term): Option[(Type, Type, Type)] = t match {
      case Constant(Seq(t1, t2, t3)) => Some((t1, t2, t3))
      case _ => None
    }
  }

  case class SpecializedTerm(override val toTerm: Term)
  extends PolymorphicTerm
  {
    def specialize(typeArguments: Type*): Term = {
      if (argumentTypesMatch(typeArguments, toTerm.getType))
        toTerm
      else {
        def arrow = =>:.arrow
        typeErrorNotTheSame(
          toTerm.toString,
          toTerm.getType,
          (typeArguments mkString s" $arrow ") + s" $arrow _")
      }
    }
  }

  // TERM BUILDERS & SUGARS

  type TermBuilder = TypingContext => PolymorphicTerm

  implicit def varBuilder(name: Name): TermBuilder =
    context => SpecializedTerm(context(name))

  implicit def createSpecializedBuilder[T <% Term](s0: T): TermBuilder =
    context => SpecializedTerm(s0)

  implicit def createPolymorphicBuilder(t0: PolymorphicTerm): TermBuilder =
    context => t0

  implicit def closeTermBuilder(builder: TermBuilder): Term =
    builder(TypingContext.empty).toTerm

  /** Convenient operator to give polymorphic terms arguments */
  implicit class BaseTermBuilderOps(t0: TermBuilder) {
    def % (typeArguments: Type*): TermBuilder =
      context =>
        SpecializedTerm(t0(context) specialize (typeArguments: _*))

    def toTerm: Term =
      t0(TypingContext.empty).toTerm
  }

  // ERROR THROWERS

  def typeErrorNotTheSame(context: String, expected: Any, actual: Any) =
    throw TypeError(s"expected $expected instead of $actual in $context")

  def typeErrorNotDefined(name : Name) =
    throw TypeError("undefined identifier " + name)
}

case class TypeError(msg: String)
extends Exception(s"Type error: $msg")

case class IDontKnow(what: String)
extends Exception(s"I don't know $what")
