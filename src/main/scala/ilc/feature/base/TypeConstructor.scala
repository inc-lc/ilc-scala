package ilc
package feature
package base

trait TypeConstructor extends functions.Types with Names {
  /** description of a type constructor with enough
    * details at the level of scala values to compute
    * its own inverse
    *
    * Example usage:
    * {{{
    * // foldNat : r → (r → r) → ℕ → r
    * object FoldNat extends ConstantWith1TypeParameter {
    *   val typeConstructor = TypeConstructor("r") { r =>
    *     r =>: (r =>: r) =>: NatType =>: r
    *   }
    * }
    * 
    * object Lookup extends ConstantWith2TypeParameters {
    *   val typeConstructor = TypeConstructor("k", "v") {
    *     case Seq(k, v) =>
    *       k =>: MapType(k, v) =>: v
    *   }
    * }
    * }}}
    */
  object TypeConstructor {
    def apply(typeVariableName: Name)
      (typeFunction: Type => Type): TypeConstructor =
      TypeConstructor(Seq(typeVariableName): _*) {
        case Seq(typeArgument) =>
          typeFunction(typeArgument)
      }
  }

  case class TypeConstructor(typeVariableNames: Name*)
    (typeFunction: Seq[Type] => Type)
  {
    /** given type arguments, compute the result, which is a type */
    def apply(typeArguments: Seq[Type]): Type =
      typeFunction(typeArguments)

    /** given the result (which is a type), compute type arguments */
    def inverse(expectedType: Type): Seq[Type] = {
      val accumulator = mkAccumulator()
      matchTypes(abstractType, expectedType, accumulator)
      postProcess(accumulator)
    }

    /** given argument types, compute type arguments */
    def inferTypeArgumentsFrom(argumentTypes: Seq[Type]): Seq[Type] = {
      val accumulator = mkAccumulator()
      // `scala.Tuple2.zipped` zips two sequences together
      // such that the result is as long as the shorter one
      // of the two sequences. Here we exploit that fact to
      // support curried syntax. No type argument needs be
      // given to a primitive with enough arguments to deduce
      // its type arguments.
      // {{{
      // val t1: Term = Maybe ! 0 ! lambda(NatType) {x => x}
      // t1.getType == MaybeType(NatType) =>: NatType
      // }}}
        (abstractArgumentTypes, argumentTypes).zipped foreach {
          (abstractType, concreteType) =>
          matchTypes(abstractType, concreteType, accumulator)
        }
      postProcess(accumulator)
    }

    // TypeVariable is chosen to be integers because
    //
    // 1. they are easily guaranteed to be unique
    //
    // 2. we can exploit the invariant `typeVariables(i).index == i`
    //    to simplify inverting the type constructor
    private[this] case class TypeVariable(index: Int) extends Type
    private[this] val typeVariables: Seq[TypeVariable] =
      typeVariableNames.zipWithIndex.map {
        case (name, index) => TypeVariable(index)
      }

    val arity = typeVariables.size

    // necessary for inversion and force early failure if
    // typeFunction is specified with incorrect arity
    private[this] val abstractType =
      typeFunction(typeVariables)
    private[this] val abstractArgumentTypes =
      getArgumentTypes(abstractType)

    private[this] def mkAccumulator(): Array[List[Type]] =
      Array.fill(arity) { Nil }
    /** match abstract type against a concrete type to obtain
      * type parameters
      *
      * CAVEAT: exploits the fact that all types are case classes of
      *         other types.  We must uphold this rule for it to not
      *         throw errors at runtime
      *
      * @param abstractType type with holes between 0 and (arity - 1)
      * @param concreteType type to match against
      * @param accumulator (inout) mutable accumulator,
      *        an `Array[List[Type]]` whose `i`th element contains
      *        all the types matched against the `i`th type variable
      *        thus far
      * @return nothing of value
      */
    private[this] def matchTypes(
      abstractType: Type,
      concreteType: Type,
      accumulator: Array[List[Type]]
    ): Unit = {
      (abstractType, concreteType) match {
        case (TypeVariable(i), _) => {
          accumulator.update(i, concreteType :: accumulator(i))
        }

        case (theAbstract: Product, theConcrete: Product) => {
          // test if the type constructor are instances of the same Scala class.
          // Since case-case inheritance is forbidden, this is enough to check
          // that classOf[theConcrete] <: classOf[theAbstract].
          if (! theAbstract.getClass.isInstance(theConcrete) ||
                theAbstract.productArity != theConcrete.productArity)
            typeErrorNotTheSame(
              s"inverting type function",
              theAbstract,
              theConcrete)
          (0 until theAbstract.productArity) foreach { i =>
            matchTypes(
              theAbstract.productElement(i).asInstanceOf[Type],
              theConcrete.productElement(i).asInstanceOf[Type],
              accumulator)
          }
        }

        // a leaf of the type AST matches, do nothing
        case _ if abstractType == concreteType =>
          ()

        case _ if abstractType != concreteType => {
          throw new TypeError(
            s"""|Cannot invert type constructor.
                |Two possible scenarios.
                |
                |1. The abstract type has children, whereas the concrete type
                |   is at a leaf. They don't match. There is a type error with
                |   the object code.
                |
                |2. There is a type of higher kind that is not a case class.
                |   please make it a case class.
                |
                |In any case, here are the types.
                |
                |abstractType =
                |  $abstractType
                |
                |concreteType =
                |  $concreteType
                |""".stripMargin)
        }
      }
    }

    /** process all info gained about type variables, report
      * their types or fail with an error
      */
    def postProcess(typeInfo: Array[List[Type]]): Seq[Type] =
      typeInfo.zipWithIndex map {
        case (Nil, i) => {
          val typeVariableName = typeVariableNames(i)
          throw new TypeError(
            s"type variable $typeVariableName unused in given arguments")
        }

        case (candidate :: otherWitnesses, i) => {
          assertTypeInfoConsistency(candidate, otherWitnesses, i)
          candidate
        }
      }

    /** checks that all accounts of a type variable are of the
      * same type */
    def assertTypeInfoConsistency(
      candidate: Type,
      otherWitnesses: List[Type],
      variableIndex: Int
    ) {
      otherWitnesses foreach { witness =>
        if (witness != candidate)
          typeErrorNotTheSame(
            "inferring the type parameter " +
              typeVariableNames(variableIndex),
            candidate,
            witness
          )
      }
    }
  }
}
