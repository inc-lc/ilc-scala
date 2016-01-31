package ilc
package examples

import feature._

/**
  * Experiments related to Koch's paper[1]. XXX currently ***VERY*** experimental.
  *
  * [1] Koch et al. (2014), *DBToaster: higher-order delta processing for dynamic, frequently fresh views*.
  */
trait DBToasterExample extends Example
   with MapReduce
   with GroupBy
   with integers.SyntaxSugar
   with integers.AbelianDerivation
   with integers.ToScala
   with equality.Syntax
   with inference.SyntaxSugar
 {

}

/**
  * This is related to example 2 in Koch's paper.
  */
class DBToasterExample2
extends DBToasterExample
{
  def dictFlatMap: UntypedTerm = ???
  //
  val dictKeyFilter: UntypedTerm =
    'valueGroup ->: 'userKeyFilter ->: 'map ->:
      FoldByHom('valueGroup, LiftGroup('valueGroup),
          'k ->: 'v ->: ifThenElse_('userKeyFilter('k), SingletonMap('k, 'v), EmptyMap),
          'map)

  //Produces a map of bags.
  val bagDictKeyFilter: UntypedTerm = dictKeyFilter(FreeAbelianGroup)

  def join: UntypedTerm = ???
  //XXX better name, eq is taken!
  val eqq: UntypedTerm = Eq

  //All of this is missing aggregation.
  def untypedProgram0 = 'r ->: 's ->: join('r, 's, 'rEl ->: 'sEl ->: eqq(Proj1('rEl), Proj2('sEl)), Pair)
  def untypedProgram =
    'r ->: 's ->: {
    letS (
      ('idxR, groupByGen(Proj1, Proj2, 'r)),
      ('idxS, groupByGen(Proj1, Proj2, 's))
    ) {
      'idxR
      //filter (first('r) == first('s)) ...
      //dictFlatMap('k ->: 'v ->: bagDictKeyFilter('k2 ->: eqq('k, 'k2), 'idxS))('idxR)
    }
    //mapFlatMap apply ('k ->: 'v ->: mapFilter('k2 ->: 'v2 ->: eqq('k, 'k2), groupByGen('s, first, second))) apply groupByGen('r, first, proj2)
  }
  override val program: Term = 'x ->: 'x //untypedProgram

  def fooMethod() = {
    println(pretty(typecheck(untypedProgram)))
  }

  //XXX to call in tests:
  //(new DBToasterExample2).fooMethod
}
