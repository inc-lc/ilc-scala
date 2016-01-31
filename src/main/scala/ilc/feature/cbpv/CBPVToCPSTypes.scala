package ilc
package feature
package cbpv

/**
 * Implement conversion from CBPV to CPS for types.
 * Incomplete.
 */
trait CBPVToCPSTypes extends TypeConversions with let.CPSTypes {
  def cbvTypeToCPS(t: Type) = valTypeToCPS(cbvTypeToCBPV(t))

  def valTypeToCPS(vt: ValType): Type = vt match {
    case UThunkVT(ct) =>
      compTypeToCPS(ct) =>: AnswerT
    //These are value types, so they aren't serious
    case UnitVT =>
      UnitType
    case SumVT(a, b) => SumType(valTypeToCPS(a), valTypeToCPS(b))
    case ProdVT(a, b) => ProductType(valTypeToCPS(a), valTypeToCPS(b))
    case BaseVT(t) => t
  }

  def compTypeToCPS(ct: CompType): Type = ct match {
    case FProducerCT(vt) =>
      valTypeToCPS(vt) =>: AnswerT
    case FunCT(srcVt, dstCt) =>
      // Tempting, but wrong:
      //valTypeToCPS(srcVt) =>: compTypeToCPS(dstCt)

      // According to the literature, we need the product of the types, which
      // happens to be the adjoint (in fact, unsurprisingly, that's how this arises).
      ProductType(valTypeToCPS(srcVt), compTypeToCPS(dstCt))
      // In fact, we can even flip the operands, to pass the continuation first
      // (and confusingly flip all other operands, but do we care?), as follows:
      //ProductType(compTypeToCPS(dstCt), valTypeToCPS(srcVt))
      //
      //However, what's the matching change for the code transformation?
      //Finally, taking continuations first (as in Fischer's transform) or last
      //(as in Plotkin's transform) makes an amazing amount of difference
      //theoretically. (See for instance Sabry and Wadler's "A Reflection on
      //Call-by-Value"; see also footnote 12 of Hatcliff and Danvy's "A Generic
      //Account of Continuation-Passing Style"). Apparently, taking
      //continuations first enables doing more administrative reductions (for
      //some purposes, too many).
    case ProdCT(a, b) =>
      ???
      //Heck, ProdCT is a pair of continuations.
      //ProductType(compTypeToType(a), compTypeToType(b))
  }
}
