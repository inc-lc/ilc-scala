package ilc
package examples

import feature._
import bags.Library._
import abelianMaps.Library._

trait GroupBy
extends abelianMaps.AbelianDerivation
   with abelianMaps.ToScala
   with bags.SyntaxSugar
   with bags.AbelianDerivation
   with bags.ToScala {

  // Even more generic version of groupByKey, more generic than Scala's groupBy.
  // A yet more generic version.

  def groupByGenMeta[T, K, V](b: Bag[T], f: T => K, g: T => V): AbelianMap[K, Bag[V]] = {
    def h(inp: => T): AbelianMap[K, Bag[V]] = singletonMap(f(inp))(bagSingleton(g(inp)))
    bagFoldGroup(liftGroup[K, Bag[V]](bags.Library.FreeAbelianGroup[V]()))(h)(b)
  }

  val groupByGen: TermBuilder =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes take 2 match {
          case Seq(t =>: k, t2 =>: v) =>
            lambda(
              Var("f", t =>: k),
              Var("g", t =>: v)) { case Seq(f, g) =>
              FoldGroup !
                (LiftGroup(k) ! FreeAbelianGroup(v)) !
                (lambda(Var("e", t)) { e =>
                  SingletonMap !
                    (f ! e) !
                    (Singleton ! (g ! e))
                })
            }
        }
    }

}

/**
  * Exemplify using GroupBy. This primitive is important because it is useful for indexing.
  */
class GroupByExample
extends Example
   with MapReduce
   with GroupBy
   with integers.SyntaxSugar
   with integers.AbelianDerivation
   with integers.ToScala
{
  val ℤ = IntType

  //Implement groupBy in the meta-language.
  def groupByMeta[T, K](b: Bag[T], f: T => K): AbelianMap[K, Bag[T]] = {
    //XXX: names of singletonMap and bagSingleton are inconsistent.
    def g(inp: => T): AbelianMap[K, Bag[T]] = singletonMap(f(inp))(bagSingleton(inp))
    bagFoldGroup(liftGroup[K, Bag[T]](bags.Library.FreeAbelianGroup[T]()))(g)(b)
  }

  // More generic version of groupByKey. This corresponds to Scala's groupBy.
  //XXX refactor with groupByGen.
  val groupBy: TermBuilder =
    new PolymorphicTerm {
      def specialize(argumentTypes: Type*): Term =
        argumentTypes take 2 match {
          case Seq(t2 =>: k, BagType(t)) =>
            lambda(Var("f", t =>: k)) { f =>
              FoldGroup !
                (LiftGroup(k) ! FreeAbelianGroup(t)) !
                (lambda(Var("e", t)) { e =>
                  SingletonMap !
                    (f ! e) !
                    (Singleton ! e)
                })
            }
        }
    }

  /*
  val program: Term =
    groupBy ofType
      (BagType(ProductType(ℤ, ℤ)) =>: (ProductType(ℤ, ℤ) =>: ℤ) =>: MapType(ℤ, BagType(ProductType(ℤ, ℤ))))
   */


  val program: Term =
    groupByGen ofType
      ((ProductType(ℤ, ℤ) =>: ℤ) =>: (ProductType(ℤ, ℤ) =>: ℤ)
        =>: BagType(ProductType(ℤ, ℤ)) =>: MapType(ℤ, BagType(ℤ)))
}
