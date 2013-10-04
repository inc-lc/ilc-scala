package ilc
package feature
package functions

import scala.language.implicitConversions
import scala.collection.GenTraversable

trait Syntax extends base.Syntax {
  case class App(operator: Term, operand: Term) extends Term {
    override def getType = (operator.getType, operand.getType) match {
      case (expectedDomain =>: range, actualDomain) =>
        if (expectedDomain == actualDomain)
          range
        else
          typeErrorNotTheSame(
            s"operand position of $operator : ${expectedDomain =>: range}",
            expectedDomain,
            s"$operand : $actualDomain")

      case _ =>
        typeErrorNotTheSame(
          "operator position",
          "function",
          operator.getType)
    }
  }

  case class Abs(variable: Variable, body: Term) extends Term {
    override def getType = variable.getType =>: body.getType
  }

  // HOW TO WRITE LAMBDA TERMS

  /** For term transformations, where the names of variables are
    * contractually binding.
    * {{{
    * val idNat = lambdaTerm(Var("x", NatType)) { Var("x", NatType) }
    *
    * derive(Abs(x, body)) = lambdaTerm(x, DVar(x)) { derive(body) }
    * }}}
    */
  def lambdaTerm(parameters: Variable*)(body: => Term): Term =
    if (parameters.isEmpty)
      body
    else
      Abs(parameters.head, lambdaTerm(parameters.tail: _*)(body))

  /** {{{
    * val idTerm = lambda { x => x }
    * }}}
    */
  def lambda(body: Name => TermBuilder): TermBuilder =
    lambda("x")(body)

  /** {{{
    * val idTerm = lambda("x") {x => x}
    * }}}
    */
  def lambda(nameSuggestion: Name)
    (body: Name => TermBuilder): TermBuilder =
    mkLambda(nameSuggestion, None)(body)

  /** Specify the argument type without specifying a name.
    * Useful for higher-order arguments.
    * {{{
    * val powerOfTwo = FoldNat ! 1 ! lambda(NatType) {x => Plus ! x ! x}
    * }}}
    */
  def lambda(argumentType: Type)
    (body: Name => TermBuilder): TermBuilder =
    mkLambda("x", Some(argumentType))(body)

  /** {{{
    * val succ = lambda(Var("n", NatType)) {n => Plus ! n ! 1}
    * }}}
    */
  def lambda(parameter: Variable)
    (body: Name => TermBuilder): TermBuilder =
    mkLambda(parameter.getName, Some(parameter.getType))(body)

  /** Basic factory of labmda abstraction term builders.
    * The name to bind is just a suggestion.
    * The argument type is optional.
    */
  private[this]
  def mkLambda(nameSuggestion: Name, argumentType: Option[Type])
    (body: Name => TermBuilder): TermBuilder =
    context => {
      val name = freshName(context, nameSuggestion)
      argumentType match {
        case Some(knownType) =>
          new SpecializedAbs(name, context, body, knownType)
        case None =>
          new ContextualAbs(name, context, body)
      }
    }

  // imports to make use of union types in multivariate lambda
  import ilc.util.UnionType._
  import scala.language.existentials

  /** Multivariate lambda with extreme measure against code duplication
    * {{{
    * val fst1 = lambda(NatType, NatType) {
    *   case Seq(x, y) => x
    * }
    * 
    * val fst2 = lambda("firstParameter", "secondParameter") {
    *   case Seq(firstParameter, secondParameter) => firstParameter
    * }
    *
    * val fst3 = lambda(Var("x", NatType), Var("y", NatType)) {
    *   case Seq(x, y) => x
    * }
    * }}}
    */
  def lambda[Parameter: Or4[Type, Name, String, Variable]#Type]
    (firstParameter: Parameter,
      secondParameter: Parameter,
      otherParameters: Parameter*)
    (body: Seq[Name] => TermBuilder): TermBuilder =
    lambda(
      firstParameter :: secondParameter :: otherParameters.toList
    )(body)

  def lambda[Parameter: Or4[Type, Name, String, Variable]#Type]
    (parameters: List[Parameter])
    (body: Seq[Name] => TermBuilder): TermBuilder =
    if (parameters.isEmpty)
      body(Nil)
    else
      parameters.head dispatchLambda { firstName =>
        lambda(parameters.tail) {
          otherNames => body(firstName :: otherNames.toList)
        }
      }

  /** After-the-fact double dispatch,
    * a part of the extreme measure against code duplication
    * in the multivariate lambda.
    *
    * If `parameter` can be called as a first argument of
    * one of the overloaded `lambda` methods,
    * then `parameter.dispatchLambda` is equivalent to
    * `lambda(parameter)`, where one of the overloaded
    * `lambda` above is chosen according to the type of
    * `parameter`.
    */
  private[this]
  implicit class ParameterOps
    [Parameter: Or4[Type, Name, String, Variable]#Type]
    (parameter: Parameter)
  {
    def dispatchLambda(body: Name => TermBuilder): TermBuilder =
      parameter match {
        case name: Name =>
          lambda(name)(body)

        case string: String =>
          lambda(string)(body)

        // if `parameter` is a `Type`, then the straightforward
        //
        //     case argumentType: Type =>
        //       lambda(argumentType)(body)
        //
        // generates the warning
        //
        // > The outer reference in this type test cannot be checked
        // > at run time.
        case _ if parameter.isInstanceOf[Type] =>
          lambda(parameter.asInstanceOf[Type])(body)

        // Ditto for variables
        case _ if parameter.isInstanceOf[Variable] =>
          lambda(parameter.asInstanceOf[Variable])(body)
      }
  }

  // TERM BUILDERS

  /** @constructor creates A type application followed by a term
    * application. In notation of Types & PLs (Pierce) page 343:
    * {{{
    * operator [operand.getType] operand
    * }}}
    *
    * @param operator is polymorphic
    * @param operand is fully specialized
    */
  case class PolymorphicApp(
    operator: PolymorphicTerm,
    operand: Term
  )
  extends PolymorphicTerm
  {
    def specialize(argumentTypes: Type*): Term = {
      val operatorTerm: Term =
        operator specialize (operand.getType +: argumentTypes: _*)
      App(operatorTerm, operand)
    }
  }

  /** Must consume at least one type argument **/
  trait TypeAbstraction extends PolymorphicTerm {
    // subclasses should implement this version of specialize instead
    // it guarantees to have at least one type argument available
    protected[this]
    def specialize(argumentType: Type, argumentTypes: Seq[Type]): Term

    def specialize(argumentTypes: Type*): Term =
      if (argumentTypes.isEmpty)
        typeErrorNotTheSame(
          s"${getClass.getSimpleName}.specialize invoked on $this",
          "type argument",
          "nothing")
      else
        specialize(argumentTypes.head, argumentTypes.tail)
 }

  case class PolymorphicAbs(name: Name, body: PolymorphicTerm)
  extends TypeAbstraction
  {
    protected[this]
    def specialize(argumentType: Type, argumentTypes: Seq[Type]): Term = {
      val variable = Var(name, argumentType)
      Abs(variable, body specialize (argumentTypes: _*))
    }
  }

  def appBuilder(operator: TermBuilder, operand: TermBuilder): TermBuilder =
    context =>
      PolymorphicApp(operator(context), operand(context).specialize(List.empty: _*))

  def absBuilder(name: Name, body: TermBuilder): TermBuilder =
    context => PolymorphicAbs(name, body(context))

  /** Swap positions of context argument and type arguments,
    * possibly with the types of some arguments already specified
    */
  class ContextualAbs(
    name: Name,
    context: TypingContext,
    body: Name => TermBuilder)
  extends TypeAbstraction
  {
    protected[this]
    def specialize(argumentType: Type, argumentTypes: Seq[Type]): Term = {
      val variable = Var(name, argumentType)
      val polymorphicBody = body(name)(variable +: context)
      Abs(variable, polymorphicBody specialize (argumentTypes: _*))
    }
    def productName = "ContextualAbs"
    override def toString =
      s"$productName($name, context = $context, $name => context => ${body(name)(context)}"
  }

  /** An abstraction where enough argument types are given
    * to type the term. When applied, the argument types are
    * checked for agreement with prior knowledge.
    */
  class SpecializedAbs(
    name: Name,
    context: TypingContext,
    body: Name => TermBuilder,
    claimedArgumentTypes: Type*)
  extends ContextualAbs(name, context, body)
  {
    override def specialize(actualArgumentTypes: Type*): Term = {
      // Use claimed arument types to specialize this term.
      // It works also when no actual argument is given
      // (e. g. when this term is a higher-order argument).
      val term: Term = super.specialize(claimedArgumentTypes: _*)
      // Check if the actual argument type agree with the claim,
      // return `term` if they agree.
      SpecializedTerm(term).specialize(actualArgumentTypes: _*)
    }
    override def productName = "SpecializedAbs"
  }

  // SUGAR

  /** Infix application to minimize parenthesis use */
  implicit class FunctionsTermBuilderOps[T <% TermBuilder](t0: T)
  {
    /** Left-associative object-level application syntax
      * Examples:
      * {{{
      * Lookup ! key ! someMap
      * Update ! key ! value ! oldMap
      * Either ! lambda(leftType) { x => ??? } ! ??? ! someSum
      * }}}
      */
    def ! (t1: TermBuilder): TermBuilder =
      appBuilder(t0, t1)

    def composeWith(t1: TermBuilder): TermBuilder =
      context => {
        val t1Term = t1(context).toTerm
        val (domain, intermediateType) = t1Term.getType match {
          case domain =>: range =>
            (domain, range)

          case wrongType =>
            typeErrorNotTheSame("composing RHS function",
              "function type",
              wrongType)
        }
        val t0Term = t0(context).specialize(intermediateType)
        (lambda(domain) { x => t0Term ! (t1Term ! x) })(TypingContext.empty)
      }

    def composeWithBuilder(t1: TermBuilder): TermBuilder =
      lambda("hole") { hole =>
        t0 ! (t1 ! hole)
      }
  }
}

trait SyntaxSugar extends Syntax {
  /** Usage: `(const ! firstArg) % ignoredType`.
    *
    * Examples:
    * {{{
    * // if given a curried argument, `const` needs only 1 type argument
    * val constSucc: Term = (const ! succ) % ℕ
    * }}}
    */
  val const: TermBuilder =
    lambda { x => lambda("ignored") { ignored => x } }

  val fst: TermBuilder =
    lambda("fstArgument", "sndArgument") { case Seq(x, y) => x }

  val snd: TermBuilder =
    lambda("fstArgument", "sndArgument") { case Seq(x, y) => y }

  /** Usage:
    * {{{
    * let_x_= {
    *   stuff
    * } { x =>
    *   blah blah x blah
    * }
    */
  def let_x_= (designation: => TermBuilder)
              (body: Name => TermBuilder): TermBuilder =
    lambda { body } ! designation
}
