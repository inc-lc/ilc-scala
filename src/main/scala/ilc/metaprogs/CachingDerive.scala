package ilc
package metaprogs

import feature._
import language._
import collection.mutable

/**
 * @author pgiarrusso
 */
trait CachingDerive {
  val mySyntax: base.Syntax with functions.Syntax with let.Syntax with products.Syntax with unit.Syntax with base.Derivation
  //= new letLanguage.Syntax //base.Syntax with functions.Syntax with let.Syntax with products.Syntax
    //with functions.Derivation with let.Derivation with base.ContextSensitiveDerivation with functions.ContextSensitiveDerivation
    //with let.Pretty {}
  import mySyntax._


  def mapName(m: scala.collection.mutable.Map[Name, Name], name: Name)(transf: String => String) = {
    //This freshening is the easiest way to ensure safety.
    //XXX: Can we avoid that and still have safety in more complex ways?
    //Generalize DeltaName to SynthesizedName? Can two ones ever conflict?
    //Would it work to use hygiene technology here?
    m.getOrElseUpdate(name, freshGen.freshName(transformName(transf)(name)))
  }

  private val freshGen = new base.FreshGen { lazy val syntax: mySyntax.type = mySyntax }

  //XXX use this technology also in main derivation.
  class NameMap(val nameMap: mutable.Map[Name, Name], nameMapper: String => String) {
    def map(name: Name): Name =
      mapName(nameMap, name)(nameMapper)
  }

  class Deriver {
    val dNameMap = new NameMap(mutable.HashMap(), "d" + _)
    val derNameMap = new NameMap(mutable.HashMap(), "der" + _)
    val pairNameMap = new NameMap(mutable.HashMap(), _ + "_p")

    //TODO: make bindings globally unique first
    val derivePVar: Var => Var = {
      case Var(name, typ) =>
        Var(dNameMap map name, deltaType(typ))
    }

    def deriveP: Term => Term = {
      def go(t: Term): Term = t match {
        case v@Var(name, typ) =>
          derivePVar(v)
        case Let(x@Var(xName, xType), App(fun, Var(funArgName, funArgType)), body) =>
          Let(derivePVar(x), App(Var(derNameMap map xName, deltaType(fun.getType)), Var(dNameMap map funArgName, deltaType(funArgType))), go(body))
      }

      go
    }
    def cacheDecl(t: Term): Term = t match {
      case Let(f, Abs(x, funBody), rest) =>
        val newFun = Abs(x, cacheExpr(Abs(derivePVar(x), deriveP(funBody)))(funBody))
        val newF = Var(f.getName, newFun.getType)
        //XXX must replace f by newF in rest.
        Let(newF, newFun, cacheDecl(rest))
      case e =>
        //XXX ??? Case not handled in the formalization — we should specify what to do
        // with top-level expression E in P.
        cacheExpr(Abs(Var(freshGen.freshName("unit"), UnitType), UnitTerm))(e)
    }

    def cacheExpr(deriv: Term)(t: Term): Term = {
      def go(t: Term): Term = t match {
        case Let(x@Var(xName, _), App(fun, funArg@Var(funArgName, funArgType)), body) =>
          val xp = pairNameMap map xName
          val derX = derNameMap map xName
          val derType = deltaType(fun.getType)
          Let(Var(xp, ProductType(x.getType, derType)), App(fun, funArg),
            Let(x, Proj1 ! xp,
              Let(Var(derX, derType), Proj2 ! xp,
                go(body))))
        case v: Var =>
          Pair ! v ! deriv
      }
      go(t)
    }
  }

  def cacheDerive(t: Term) = (new Deriver).cacheDecl(t)
}

//object CachingDerive extends CachingDerive
