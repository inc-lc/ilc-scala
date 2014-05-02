package ilc
package examples

import feature._

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
 * See Koch et al. (2014), *DBToaster: higher-order delta processing for dynamic, frequently fresh views*.
 * This is related to example 2.
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
  def untypedProgram0 = 'r ->: 's ->: join('r, 's, 'rEl ->: 'sEl ->: eqq(proj1('rEl), proj1('sEl)), pair)
  def untypedProgram =
    'r ->: 's ->: {
    letS (
      ('idxR, groupByGen(proj1, proj2, 'r)),
      ('idxS, groupByGen(proj1, proj2, 's))
    ) {
      'idxR
      //filter (proj1('r) == proj1('s)) ...
      //dictFlatMap('k ->: 'v ->: bagDictKeyFilter('k2 ->: eqq('k, 'k2), 'idxS))('idxR)
    }
    //mapFlatMap apply ('k ->: 'v ->: mapFilter('k2 ->: 'v2 ->: eqq('k, 'k2), groupByGen('s, proj1, proj2))) apply groupByGen('r, proj1, proj2)
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
