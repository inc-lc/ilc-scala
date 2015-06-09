package ilc
package metaprogs

import feature._

trait AlphaEquiv {
  outer: functions.Syntax =>
  //Use custom name to avoid conflicts
  protected val aeFreshGen = new base.FreshGen { val syntax: outer.type = outer }
  import aeFreshGen.freshName

  def alphaEquiv(t1: Term, t2: Term, ignoreTypes: Boolean = false): Boolean =
    doAlphaEquiv(t1, t2, Map(), Map(), ignoreTypes)
  //The maps should be from Name to Name
  protected def doAlphaEquiv(t1: Term, t2: Term, map1: Map[Name, Name], map2: Map[Name, Name], ignoreTypes: Boolean): Boolean =
    (t1, t2) match {
      case (Abs(Var(n1, t1), body1), Abs(Var(n2, t2), body2)) =>
        (ignoreTypes || t1 == t2) && {
          val nNew = freshName(n1)
          doAlphaEquiv(body1, body2, map1 + (n1 -> nNew), map2 + (n2 -> nNew), ignoreTypes)
        }
      case (App(f1, arg1), App(f2, arg2)) =>
        doAlphaEquiv(f1, f2, map1, map2, ignoreTypes) && doAlphaEquiv(arg1, arg2, map1, map2, ignoreTypes)
      case (v1 @ Var(n1, t1), v2 @ Var(n2, t2)) =>
         (ignoreTypes || t1 == t2) && map1(n1) == map2(n2)
      case (Constant(pc1, tt1), Constant(pc2, tt2)) =>
        pc1 == pc2 && (ignoreTypes || tt1 == tt2)
      case _ =>
        //Remaining values, that do *not* include type arguments.
        //So this *will* ignore types.
        t1 == t2
    }
}

trait AlphaEquivLet extends AlphaEquiv {
  outer: let.Syntax =>
  import aeFreshGen.freshName

  override protected def doAlphaEquiv(t1: Term, t2: Term, map1: Map[Name, Name], map2: Map[Name, Name], ignoreTypes: Boolean): Boolean =
    (t1, t2) match {
      case (Let(Var(n1, t1), e1, body1), Let(Var(n2, t2), e2, body2)) =>
        (ignoreTypes || t1 == t2) &&
        doAlphaEquiv(e1, e2, map1, map2, ignoreTypes) &&
        {
          val nNew = freshName(n1)
          doAlphaEquiv(body1, body2, map1 + (n1 -> nNew), map2 + (n2 -> nNew), ignoreTypes)
        }
      case _ =>
        super.doAlphaEquiv(t1, t2, map1, map2, ignoreTypes)
    }
}
