package ilc
package howTo

import feature._
import org.scalatest._

/** How to write simply-typed lambda terms
  * in the presence of simple right-to-left type inference
  */

class WriteLambdaTerms
extends FlatSpec with Matchers
   with functions.Pretty
   with integers.Syntax
   with products.Syntax
   with bags.Syntax
   with sums.Syntax
   with maps.Types // maps included for illustration only
{
  // SHORT GUIDE

  // The object language of paper 63 is simply typed lambda calculus.
  // Syntax trees of the object language can always be constructed
  // if the 3 rules below are observed.
  // 
  //
  // t ::= x                 x           // (Scala identifier)
  //     | t₁ t₂             t1 ! t2
  //     | λx : τ. t         lambda(τ) { x => t }
  //
  // Rule 1: Declare the type of syntax trees as `Term`.
  // Rule 2: Annotate the argument type of lambda abstractions.
  // Rule 3: Fully instantiate polymorphic constants on each use.
  //
  // Short examples:

  val id_Int = lambda(IntType) { x => x }

  "id_Int" should "be a function between integers" in {
    assert(id_Int.getType == (IntType =>: IntType))
  }

  val insert_Int =
    lambda(IntType) { n =>
      lambda(BagType(IntType)) { bag =>
        Union(IntType) ! (Singleton(IntType) ! n) ! bag
      }
    }

  "insert_Int" should "have type Int => Bag[Int] => Bag[Int]" in {
    assert(
      insert_Int.getType
        ==
      (IntType =>: BagType(IntType) =>: BagType(IntType))
    )
  }

  // LONG GUIDE
  //
  // - Representation of abstract syntax trees
  // - How & in what situation can one write less type annotations

  // The object language of paper 63 is simply typed lambda calculus.
  // In order for bottom-up visitors to know the type of the subterm
  // being traversed over at all times, we choose to annotate every
  // occurrence of a variable with its type.
  //
  //   case Var(name: Name, getType: Type)
  //
  // Here is an example of a term with all annotations present.
  // It is the uncurried union of bags of integers (here and elsewhere,
  // symbols in human-readable syntax carry the default meaning of
  // their definitions in Agda standard library. For example,
  // `Int × Int` is the type of pairs of integers, and `Int → Int`
  // is the type of unary integer functions).
  //
  //   uncurriedUnion : Bag Int × Bag Int → Bag Int

  val bagOfInts  = BagType(IntType)
  val pairOfBags = ProductType(bagOfInts, bagOfInts)

  val uncurriedUnionWithFullTypeAnnotation: Term =
    Abs(Var("pair", pairOfBags),
      App(
        App(
          Union(IntType),
          App(
            Proj1(bagOfInts, bagOfInts),
            Var("pair", pairOfBags))),
        App(
          Proj2(bagOfInts, bagOfInts),
          Var("pair", pairOfBags))))

  // Writing full annotation by hand is tedious and error-prone.
  // We developed a simple right-to-left algorithm to infer the
  // type annotations of variables (among other things). In
  // retrospect, full type inference by unification might have
  // been a better choice.
  //
  // right-to-left type inference is available in the body of any
  // subclass of
  //
  //   ilc.feature.functions.SyntaxSugar
  //
  // We use the exclamation mark (!) as infix contructor of
  // applications, because Scala associates it to the left.
  //
  // It is very important to declare terms built using type
  // inference as having the Scala type "Term"---it would
  // trigger the implicit conversion that puts type annotations
  // in their place.

  val uncurriedUnion: Term =
    lambda(Var("pair", pairOfBags)) { pair =>
      Union ! (Proj1 ! pair) ! (Proj2 ! pair)
    }

  val inference = "Right-to-left type inference"

  inference should "put type annotations in the correct places" in {
    assert(uncurriedUnion == uncurriedUnionWithFullTypeAnnotation)
  }

  // This is the correspondence between the grammar of
  // simply-typed lambda calculus and the syntax of our
  // AST construction DSL.
  //
  // t ::= x                 x           // (Scala identifier)
  //
  //     | t₁ t₂             t1 ! t2
  //
  //     | λx : τ. t         lambda(Var("x"), τ) { x => t }
  //                         --or--
  //                         lambda(τ) { x => t }
  //
  // The name given to `lambda` constructor is only a suggestion
  // and is subject to alpha-renaming. If the meaning of the
  // actual name bound by λ is unimportant, we can omit it.

  val uncurriedUnionWithoutNameSuggestion: Term =
    lambda(pairOfBags) { pair =>
      Union ! (Proj1 ! pair) ! (Proj2 ! pair)
    }

  // In a nutshell, the type inference algorithm computes the
  // type of operands from bottom up and use that knowledge to
  // annotate formal parameters of Operators. In particular,
  // we can omit the argument type annotation in a β-redex.

  val five: Term = (lambda { x => x }) ! LiteralInt(5)

  inference should "infer the type of `id` applied to an integer" in {
    assert(five.getType == IntType)
  }

  // However, no information flows from operators to operands;
  // higher-order arguments in particular need enough annotations
  // for the inference algorithm to fully obtain its type.

  import ilc.feature.base.TypeError

  inference should "have to know the type of arguments completely" in {
    intercept[TypeError] {
      val uninferrable: Term = lambda { x => x }
    }

    val id_Int2Int = lambda(IntType =>: IntType) { x => x }

    assert(
      id_Int2Int.getType
        ==
      ((IntType =>: IntType) =>: (IntType =>: IntType))
    )

    intercept[TypeError] {
      val uninferrable: Term = id_Int2Int ! (lambda { x => x })
    }
  }

  // Polymorphic constants are effectively constants with
  // System F types, who have to embed themselves in simply
  // typed lambda calculus by instantiating all quantified
  // type variables to monotypes. Take for example
  // `ilc.feature.abelianMaps.Syntax.SingletonMap`:
  //
  //   singletonMap : ∀k v. k → v → Map k v

  // (copied from src/main/scala/ilc/feature/abelianMaps/Syntax.scala)
  object SingletonMap extends ConstantWith2TypeParameters {
    val typeConstructor =
      TypeConstructor("keyType", "valType") {
        case Seq(keyType, valType) =>
          keyType =>: valType =>: MapType(keyType, valType)
      }
  }

  // In System F, we would call
  //
  //   singletonMap [Int] [Int]  :  Int → Int → Map Int Int
  //
  // to specialize `singletonMap` to one simply-typed function.
  // In the Scala AST, type application is encoded as Scala
  // function call with types as arguments.

  "Type applications on SingletonMap" should "be function calls" in {
    assert(
      SingletonMap(IntType, IntType).getType
        ==
      (IntType =>: IntType =>: MapType(IntType, IntType))
    )
  }

  // If a polymorphic constant is applied to enough arguments
  // to fully infer the necessary type applications, then
  // we can omit the type applications.

  inference should
  "be able to infer type applications given enoguh information" in {
    val singleton: Term = SingletonMap ! LiteralInt(5) ! PlusInt
    assert(
      singleton.getType ==
        MapType(IntType, IntType =>: IntType =>: IntType)
    )
  }

  // If immediate arguments of a polymorphic constant don't
  // provide enough information to infer all type applications,
  // then we cannot create a simply typed term from that
  // constant.

  inference should
  "give up if argument types don't provide enough information" in {
    intercept[TypeError] {
      val valueTypeUnknown: Term = SingletonMap ! LiteralInt(5)
    }
  }

  // Of course, type applications of some polymorphic constants
  // can never be inferred from argument types.

  inference should "give up on a constructor of sum types" in {
    intercept[TypeError] {
      val sumOfIntAndUnknown: Term = Inj1 ! LiteralInt(5)
    }
  }

  // We hope the information herein is sufficient for decoding
  // object level programs in `ilc.examples.*`.
}
