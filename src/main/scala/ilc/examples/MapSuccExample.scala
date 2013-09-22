package ilc
package examples

import language.bacchus

class MapSuccExample extends MapSuccBaseExample {
  override def derivative: Term = dmap_s1 ! constSucc ! (derive(constSucc))

  // hand-crafted derivative of map with the first argument stable
  // specialized to the type (ℕ → ℕ → ℕ) → Map ℕ ℕ → Map ℕ ℕ
  def dmap_s1: Term = {
    val mType = MapType(ℕ, ℕ)
    val fVar = Var("f", ℕ =>: ℕ =>: ℕ)
    val mVar = Var("m", mType)
    lambda(fVar, DVar(fVar), mVar, DVar(mVar)) { case Seq(f, df, m, dm) =>
      Either !
        // if the change is insertion/deletion/modification,
        // then map over the changes and see what happens.
        (Inj1(mType) composeWith (mapWithKey ! lambda(Var("key", ℕ)) { key =>
          Either !
            // case del/ins
            // -- on deletion, delete it anyway
            // -- on insertion, insert the new value
            (Inj1(ℕ) composeWith (Maybe !
              Nope(ℕ) !
              (Just composeWith (f ! key))
            )) !
            // case update:
            // 1. apply the given derivative `df`
            // 2. wrap result change in an ins/del/mod type
            (Inj2(MaybeType(ℕ)) composeWith (df ! key ! key ! (
              // TODO: replace 1997 by ErrorTerm
              // TODO2: lift this maybe-free lookup term as sugar
              Maybe ! 1997 !
                lambda(ℕ) { x => x } !
                (Lookup ! key ! m)
            )))
        })) !
        // if the change is a replacement,
        // then increment the replacement.
        lambda(mType) { newMap =>
          mkMapReplacement(mapWithKey ! f ! newMap)
        } !
        dm
    }
  }
}
