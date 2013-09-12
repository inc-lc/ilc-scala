package ilc
package feature
package functions

trait Typing
extends base.Typing
   with Attribution
{
  typingTrait: TypedSyntax =>

  type Context = Map[Var, Type]
  def emptyContext: Context = Map.empty[Var, Type]

  // the typeOf method is an obligation imposed by base.Typing
  // to ensure that functions.Typing can type (closed) terms
  // introduced by feature.functions. In practice, it is always
  // better to use TypingAttr instead.

  override def typeOf(t: Term): Type =
    typeOf(t, emptyContext)

  def typeOf(t: Term, initialContext: Context): Type = {
    val attr = TypingAttr(t, initialContext)
    attr(attr.rootSubterm)
  }

  // Warning: not extensible code. This does not compose with extensions of Term.
  case class TypingAttr(root: Term, initialContext: Context = emptyContext)
  extends CachedAttribute[Type](root)
  {
    val context = ContextAttr(root, initialContext)

    def init(s: Subterm): Unit = {
      val sType: Type = s.term match {
        case TypedAbs(argumentName, argumentType, body) => {
          s.children map init
          argumentType =>: lookup(s.children.head)
        }
        case abs: Abs =>
          throw new UntypedAbsError(abs)

        case app: App => {
          s.children map init
          s.children map lookup match {
            case List(sigma =>: tau, argType) if sigma == argType =>
              tau
            case types =>
              throw new BadAppError(types.head, types.last, app)
          }
        }

        case x: Var => context(s) get x match {
          case None =>
            throw new UntypedVarError(x)
          case Some(myType) =>
            myType
        }

        case Const(c) =>
          typeOf(c)

        // The last case will never trigger, because MatchError
        // will be thrown when constructing a context inside the
        // creation of Attribute.rootSubterm.
        case t =>
           sys error  s"Don't know how to compose type systems:\n    $t"
      }
      update(s, sType)
    }

    init(rootSubterm)
  }

  case class ContextAttr(root: Term, initialContext: Context = emptyContext)
  extends InheritedAttribute[Context](root, initialContext)
  {
    def inherit(parent: Subterm, parentAttr: Context) = parent.term match {
      case TypedAbs(name, argumentType, body) =>
        List(parentAttr.updated(Var(name), argumentType))

      case _: App =>
        List(parentAttr, parentAttr)

      case abs: Abs =>
        throw new UntypedAbsError(abs)
    }
  }

  abstract class TypingError(entity: String, t: Any)
  extends Exception(s"unable to type $entity:\n    $t")

  class UntypedAbsError(abs: Abs)
  extends TypingError("an abstraction without argument type", abs)

  class UntypedVarError(x: Var)
  extends TypingError("a variable outside the typing context", x)

  class BadAppError(operatorType: Type, operandType: Type, app: App)
  extends
    TypingError(s"application of ($operatorType) to ($operandType)", app)
}
