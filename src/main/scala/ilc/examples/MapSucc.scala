package ilc
package examples

import language.bacchus

trait MapSucc
extends Archive
{
  // Example 3 variant: mapping over the values of a map
  // program = map succ
  // input = Map(1 -> 1, 2 -> 2, 3 -> 3)
  // program input = Map(1 -> 2, 2 -> 3, 3 -> 4)
  //
  // The names of examples can be used elsewhere to access
  // a particular example. Rename wisely.
  class MapSuccExample
  extends Example
     with bacchus.Syntax // for syntactic sugars
     with bacchus.ToScala
     with bacchus.BasicDerivation
  {

    def program: Term = mapWithKey ! succ

    override def derivative: Term = dmap_s1 ! succ ! (derive(succ))

    def succ: Term = lambda(ℕ) { keyIgnored => Plus ! 1 }

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

    // alias
    private[this] val ℕ = NatType
  }

  // the compiled object is "MapSuccBinary"
  // the benchmarking object is "MapSuccBenchmark"
  addExample("MapSucc", new MapSuccExample)
}
