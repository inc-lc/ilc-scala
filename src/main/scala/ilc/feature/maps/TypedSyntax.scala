package ilc
package feature
package maps

trait TypedSyntax
extends Syntax
   with Typing
   with base.TypedSyntax
{
  object TypedEmptyMap {
    def apply(keyType: Type, valType: Type): Term =
      TypedConst(EmptyMap, MapType(keyType, valType))

    def unapply(t: Term): Option[(Type, Type)] = t match {
      case TypedConst(EmptyMap, MapType(keyType, valType)) =>
        Some((keyType, valType))

      case _ =>
        None
    }
  }

  object TypedUpdate {
    def apply(k: Type, v: Type): Term =
      TypedConst(Update, k =>: v =>: MapType(k, v) =>: MapType(k, v))

    def unapply(t: Term): Option[(Type, Type)] = t match {
      case TypedConst(Update, k =>: v =>: _) =>
        Some((k, v))

      case _ =>
        None
    }
  }

  object TypedLookup {
    def apply(k: Type, v: Type): Term =
      TypedConst(Lookup, k =>: MapType(k, v) =>: v)

    def unapply(t: Term): Option[(Type, Type)] = t match {
      case TypedConst(Lookup, k =>: _ =>: v) =>
        Some((k, v))

      case _ =>
        None
    }
  }

  object TypedFold {
    def apply(k: Type, a: Type, b: Type): Term =
      TypedConst(Fold, (k =>: a =>: b =>: b) =>: b =>: MapType(k, a) =>: b)

    def unapply(t: Term): Option[(Type, Type, Type)] = t match {
      case TypedConst(Fold, (k =>: a =>: b =>: _) =>: _) =>
        Some((k, a, b))

      case _ =>
        None
    }
  }
}
