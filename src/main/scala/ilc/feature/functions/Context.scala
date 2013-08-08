package ilc
package feature.functions

/**
 * Context = hole
 *         | Context operand
 *         | operator Context
 *         | λ x . Context
 *
 * Implemented in Zipper-style reported by Huet, except contexts
 * are represented top-down and not bottom-up, for easier
 * understanding and slower evaluation.
 *
 * Gérard Huet.
 * The Zipper.
 * Journal of Functional Programming 7 (5): 549--554, 1997.
 */

trait Context { self: Syntax =>
  sealed trait Context {
    // put a term in the hole
    def apply(t: Term): Term

    // put a context in the hole
    def apply(ctx: Context): Context

    // separate the context into C₁, C₂ such that
    // 1. self == C₁[C₂]
    // 2. C₁ is 1-layer thin
    def splitTop: (Context, Context)

    // separate the context into C₁, C₂ such that
    // 1. self == C₁[C₂]
    // 2. C₂ is 1-layer thin
    def splitBot: (Context, Context)
  }

  object Context {
    case object Hole extends Context {
      def apply(t: Term) = t
      def apply(ctx: Context) = ctx
      def splitTop = sys error "can't split a hole from top"
      def splitBot = sys error "can't split a hole from bottom"
    }

    case class App1(operator: Context, operand: Term)
    extends Context {
      def apply(t: Term) = operator(t)(operand)
      def apply(ctx: Context) = App1(operator(ctx), operand)
      def splitTop = (App1(Hole, operand), operator)
      def splitBot = operator match {
        case Hole => (Hole, this)
        case _ => {
          val (operatorTop, operatorBot) = operator.splitBot
          (App1(operatorTop, operand), operatorBot)
        }
      }
    }

    case class App2(operator: Term, operand: Context)
    extends Context {
      def apply(t: Term) = operator(operand(t))
      def apply(ctx: Context) = App2(operator, operand(ctx))
      def splitTop = (App2(operator, Hole), operand)
      def splitBot = operand match {
        case Hole => (Hole, this)
        case _ => {
          val (operandTop, operandBot) = operand.splitBot
          (App2(operator, operandTop), operandBot)
        }
      }
    }

    case class Abs(name: String, body: Context)
    extends Context {
      def apply(t: Term) = name ->: body(t)
      def apply(ctx: Context) = Abs(name, body(ctx))
      def splitTop = (Abs(name, Hole), body)
      def splitBot = body match {
        case Hole => (Hole, this)
        case _ => {
          val (bodyTop, bodyBot) = body.splitBot
          (Abs(name, bodyTop), bodyBot)
        }
      }
    }
  }

  case class Subterm(term: Term, context: Context) {
    import Context.Hole

    def root = context(term)

    def parent: Subterm = {
      val (ancestors, parent) = context.splitBot
      Subterm(parent(term), ancestors)
    }

    val children: List[Subterm] = term match {
      case App(s, t) =>
        List(Subterm(s, context(Context.App1(Hole, t))),
             Subterm(t, context(Context.App2(s, Hole))))
      case Abs(name, body) =>
        List(Subterm(body, context(Context.Abs(name, Hole))))
      case Const(_) | Var(_) =>
        Nil
    }

    def eachChild[U](f: Subterm => U): Unit =
      children.foreach(f)
  }

  object Subterm {
    // s is the subterm of s itself
    def refl(s: Term) = Subterm(s, Context.Hole)
  }
}
