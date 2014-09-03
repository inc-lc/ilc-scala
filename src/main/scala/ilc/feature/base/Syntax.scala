package ilc
package feature
package base

import scala.language.implicitConversions
import scala.language.postfixOps

trait Syntax
extends TypeConstructors
{
  trait Typed {
    /** Returns type or fails horribly. */
    val getType: Type
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

  case class Var(getName: Name, getType: Type) extends Term

  //To override with pretty if pretty is available.
  protected def showTerm(t: Term): String = t.toString

  import collection.immutable.HashMap

  // TYPING CONTEXT
  //This is a case class just to get equals generated.
  case class TypingContext(private val vars: Map[Name, Var]) {
    def lookup(name: Name): Option[Var] =
      this.vars get name

    /** find the name, or die **/
    def apply(name: Name): Var =
      lookup(name).fold(typeErrorNotInContext(name))(identity)

    /** membership test */
    def contains(name: Name): Boolean =
      lookup(name).fold(false)(_ => true)

    private def toEntry(v: Var) =
      v.getName -> v

    /** add a (typed) variable to the typing context */
    def +: (variable: Var): TypingContext =
      TypingContext(this.vars + toEntry(variable))

    def ++(variables: Traversable[Var]): TypingContext =
      TypingContext(this.vars ++ (variables map toEntry))
  }

  object TypingContext {
    val empty: TypingContext = TypingContext(HashMap.empty[Name, Var])
    def apply(variables: Var*): TypingContext =
      TypingContext.empty ++ variables
  }

  /**
   * Extract the base name from an indexed one.
   * This is useful to prevent creating nested indexed names - they easily get
   * nested enough to cause StackOverflowExceptions with recursive algorithms.
   */
  def decomposeName(n: Name): (NonIndexedName, Int) =
    n match {
      case IndexedName(orig, idx) =>
        (orig, idx)
      case nin: NonIndexedName =>
        (nin, 0)
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
  def freshName(context: TypingContext, _default: Name): Name = {
    val (default, startIdx) = decomposeName(_default)
    var newName: Name = default
    var index = startIdx
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
      * In general, it needs to get *all* argument types,
      * but this requirement can be relaxed by subtypes (in
      * particular, for polymorphic constants).
      *
      * Implementation of this method is the sole obligation
      * of subclasses
      *
      * @param argumentTypes types of future arguments
      * @return a term builder free from type abstractions
      */
    def specialize(argumentTypes: Type*): Term

    /** give the full type of a term */
    def ofType(expectedType: Type): Term = {
      specialize(getArgumentTypes(expectedType): _*)
    }

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

    override def ofType(expectedType: Type): Term = {
      val typeArguments = typeConstructor.inverse(expectedType)
      val term = apply(typeArguments: _*)
      require(term.getType == expectedType)
      term
    }

    /** the constant with all type parameters supplied */
    protected[this] case class Constant(typeArguments: Seq[Type])
    extends Term
    {
      lazy val getType: Type = typeConstructor(typeArguments)

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
    def specialize(argumentTypes: Type*): Term = {
      case object Underscore extends Type { override def toString = "_" }

      if (argumentTypesMatch(argumentTypes, toTerm.getType))
        toTerm
      else {
        val term = toTerm.toString
        val actual = toTerm.getType
        val expected = argumentTypes.foldRight(Underscore: Type)(_ =>: _).toString

        typeErrorWrongType(term, actual, expected)
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
  implicit class BaseTermBuilderOps[T <% TermBuilder](t: T) {
    // activates implicit conversion
    val t0: TermBuilder = t

    /**
      * Specify explicitly the types of arguments to this term.
      *
      * Give enough argument type to fully specialize
      * a polymorphic term builder.
      *
      * In general, it needs to get *all* argument types,
      * but this requirement is relaxed for polymorphic
      * constants. See documentation of
      * `PolymorphicTerm.specialize`.
      */
    def % (argumentTypes: Type*): TermBuilder =
      context =>
        SpecializedTerm(t0(context) specialize (argumentTypes: _*))

    /** Type ascription */
    def ofType(expectedType: Type): TermBuilder =
      context =>
        SpecializedTerm(t0(context) ofType expectedType)

    /** Finish building a closed term, starting from the empty context */
    def toTerm: Term =
      t0(TypingContext.empty).toTerm
  }
}

/**
 * A reusable freshname generator. Since this contains mutable state, this module
 * is designed to be imported via composition, not by mixing it in.
 */
trait FreshGen {
  val syntax: Syntax
  import syntax._

  //Have a very simple and reliable fresh variable generator. Tracking free
  //variables might have been the performance killer of the other normalizer.
  var index = 0
  def resetIndex() {
    index = 0
  }

  def fresh(varName: Name, varType: Type): Var = {
    index += 1
    //Var(IndexedName("z", index), varType)
    val compress = false
    val baseName =
      if (compress)
        "z": NonIndexedName
      else
        //Keep names, for extra readability.
        decomposeName(varName)._1
    Var(IndexedName(baseName, index), varType)
  }

  def fresh(v: Var): Var = fresh(v.getName, v.getType)
}
