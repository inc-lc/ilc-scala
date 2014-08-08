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
      foldByHom('valueGroup, liftGroup('valueGroup),
          'k ->: 'v ->: ifThenElse_('userKeyFilter('k), singletonMap('k, 'v), EmptyMap),
          'map)

  //Produces a map of bags.
  val bagDictKeyFilter: UntypedTerm = dictKeyFilter(freeAbelianGroup)

  def join: UntypedTerm = ???
  //XXX better name, eq is taken!
  val eqq: UntypedTerm = Eq

  //All of this is missing aggregation.
  def untypedProgram0 = 'r ->: 's ->: join('r, 's, 'rEl ->: 'sEl ->: eqq(first('rEl), first('sEl)), pair)
  def untypedProgram =
    'r ->: 's ->: {
    letS (
      ('idxR, groupByGen(first, second, 'r)),
      ('idxS, groupByGen(first, second, 's))
    ) {
      'idxR
      //filter (first('r) == first('s)) ...
      //dictFlatMap('k ->: 'v ->: bagDictKeyFilter('k2 ->: eqq('k, 'k2), 'idxS))('idxR)
    }
    //mapFlatMap apply ('k ->: 'v ->: mapFilter('k2 ->: 'v2 ->: eqq('k, 'k2), groupByGen('s, first, second))) apply groupByGen('r, first, proj2)
  }
  override val program: Term = 'x ->: 'x //untypedProgram

  def fooMethod() = try {
    println(pretty(untypedProgram))
  } catch { case e: inference.Inference#UnificationFailure =>
    println(e.details)
  }

  //XXX to call in tests:
  //(new DBToasterExample2).fooMethod
}
