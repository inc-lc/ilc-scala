package ilc
package feature
package let

object StaticCaching {
  import WorksheetHelpers._
  import bacchus._
  import cacher._
  import aNormalizer._
  /*
  val testBeta1 =
    'f2 ->:
	    letS(
	      'f -> ('x ->: 'y ->: 'f2('x)('y)),
	      'f1 -> ('x ->: 'y ->: pair('x)('y)),
	      'h -> ('x ->: 'y ->: let('g, 'f('x))(PlusInt('g('y))('g('y))))
	    ) { 'h }
  show(normalize(testBeta1))
  val testBeta1Bis =
    'f2 ->:
      letS(
        'f -> ('x ->: 'y ->: 'f2('x)('y)),
        'h -> ('x ->: 'y ->:
          letS(
            'g -> 'f('x),
            'i -> 'g('y)
          ) {
              PlusInt('i)('i)
            })
      ) { 'h }
  show(normalize(testBeta1Bis))
  val testBeta2 =
    'f ->:
      letS(
        'h -> ('x ->: 'y ->: let('g, 'f('x))('g('y)))
      ) { 'h }
  show(normalize(testBeta2))
  val testBeta3 =
    'f ->:
      letS(
        'h -> ('x ->: 'y ->: let('g, 'f('x))(PlusInt('g('y))('g('y))))
      ) { 'h }
  show(normalize(testBeta3))
  show(aNormalizeTerm(normalize(testBeta3)))
  */
  val test1 =
    letS(
      'id -> ('x ->: 'x),
      'id_i -> ('x ->: 'x),
      'id_i2 -> ('x ->: 'x),
      'apply -> ('f ->: 'x ->: 'f('x))
    ) {
        'id('apply, 'id_i, 'id_i2(3: Term))
      }                                           //> test1  : ilc.feature.let.WorksheetHelpers.bacchus.UntypedTerm = ULet(id,UAb
                                                  //| s(x,None,UVar(x)),ULet(id_i,UAbs(x,None,UVar(x)),ULet(id_i2,UAbs(x,None,UVa
                                                  //| r(x)),ULet(apply,UAbs(f,None,UAbs(x,None,UApp(UVar(f),UVar(x)))),UApp(UApp(
                                                  //| UApp(UVar(id),UVar(apply)),UVar(id_i)),UApp(UVar(id_i2),UMonomorphicConstan
                                                  //| t(LiteralInt(3))))))))
  //val t = test1
  val test3 =
    let('x, ifThenElse(True, 1, 2): Term)('x)     //> test3  : ilc.feature.let.WorksheetHelpers.bacchus.UntypedTerm = ULet(x,UMon
                                                  //| omorphicConstant(App(App(App(IfThenElse(ℤ),True),Abs(Var(unit,UnitType),L
                                                  //| iteralInt(1))),Abs(Var(unit,UnitType),LiteralInt(2)))),UVar(x))
  show(aNormalizeTerm(test3))                     //> res0: String = "
                                                  //| a_1 =
                                                  //|   IfThenElse(ℤ);
                                                  //| a_2 =
                                                  //|   a_1
                                                  //|     True;
                                                  //| a_3 =
                                                  //|   λunit.
                                                  //|     LiteralInt(1);
                                                  //| a_4 =
                                                  //|   a_2
                                                  //|     a_3;
                                                  //| a_5 =
                                                  //|   λunit.
                                                  //|     LiteralInt(2);
                                                  //| a_6 =
                                                  //|   a_4
                                                  //|     a_5;
                                                  //| a_6"
  /*
  show(test3)
  show(etaExpandPrimitives(test3))
  show(aNormalizeTerm(etaExpandPrimitives(test3)))
  */
  show(addCaches(test3))                          //> res1: String = "
                                                  //| a_7 =
                                                  //|   λeta_1.
                                                  //|     Pair((UnitType → ℤ) → ProductType((UnitType → ℤ) → ProductT
                                                  //| ype(ProductType(ℤ,UnitType),UnitType),UnitType), UnitType)
                                                  //|       (λeta_2.
                                                  //|          Pair((UnitType → ℤ) → ProductType(ProductType(ℤ,UnitType),
                                                  //| UnitType), UnitType)
                                                  //|            (λeta_3.
                                                  //|               Pair(ProductType(ℤ,UnitType), UnitType)
                                                  //|                 (Pair(ℤ, UnitType)
                                                  //|                    (IfThenElse(ℤ)
                                                  //|                       eta_1
                                                  //|                       eta_2
                                                  //|                       eta_3)
                                                  //|                    UnitTerm)
                                                  //|                 UnitTerm)
                                                  //|            UnitTerm)
                                                  //|       UnitTerm;
                                                  //| aTot_8 =
                                                  //|   a_7
                                                  //|     True;
                                                  //| a_8 =
                                                  //|   Proj1((UnitType → ℤ) → (UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_8;
                                                  //| a_9 =
                                                  //|   λunit.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       LiteralInt(1)
                                                  //|       UnitTerm;
                                                  //| aTot_10 =
                                                  //|   a_8
                                                  //|     a_9;
                                                  //| a_10 =
                                                  //|   Proj1((UnitType → ℤ) → ℤ, UnknownType())
                                                  //|     aTot_10;
                                                  //| a_11 =
                                                  //|   λunit.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       LiteralInt(2)
                                                  //|       UnitTerm;
                                                  //| aTot_12 =
                                                  //|   a_10
                                                  //|     a_11;
                                                  //| a_12 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_12;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|  λx5lit.
                                                  //|  λx6lit.
                                                  //|  λx7lit.
                                                  //|    Pair(ℤ, ProductType(ProductType(ℤ,UnknownType()),ProductType(UnitTyp
                                                  //| e → ℤ,ProductType(ProductType((UnitType → ℤ) → ℤ,UnknownType())
                                                  //| ,ProductType(UnitType → ℤ,ProductType(ProductType((UnitType → ℤ) �
                                                  //| � (UnitType → ℤ) → ℤ,UnknownType()),BooleanType → (UnitType → �1480 ��) → (UnitType → ℤ) → ℤ))))))
                                                  //|      x1lit
                                                  //|      (Pair(ProductType(ℤ,UnknownType()), ProductType(UnitType → ℤ,Pro
                                                  //| ductType(ProductType((UnitType → ℤ) → ℤ,UnknownType()),ProductType(
                                                  //| UnitType → ℤ,ProductType(ProductType((UnitType → ℤ) → (UnitType �1480 �� ℤ) → ℤ,UnknownType()),BooleanType → (UnitType → ℤ) → (Unit
                                                  //| Type → ℤ) → ℤ)))))
                                                  //|         x2lit
                                                  //|         (Pair(UnitType → ℤ, ProductType(ProductType((UnitType → ℤ) 
                                                  //| → ℤ,UnknownType()),ProductType(UnitType → ℤ,ProductType(ProductType
                                                  //| ((UnitType → ℤ) → (UnitType → ℤ) → ℤ,UnknownType()),BooleanTy
                                                  //| pe → (UnitType → ℤ) → (UnitType → ℤ) → ℤ))))
                                                  //|            x3lit
                                                  //|            (Pair(ProductType((UnitType → ℤ) → ℤ,UnknownType()), Pro
                                                  //| ductType(UnitType → ℤ,ProductType(ProductType((UnitType → ℤ) → (U
                                                  //| nitType → ℤ) → ℤ,UnknownType()),BooleanType → (UnitType → ℤ) 
                                                  //| → (UnitType → ℤ) → ℤ)))
                                                  //|               x4lit
                                                  //|               (Pair(UnitType → ℤ, ProductType(ProductType((UnitType →
                                                  //|  ℤ) → (UnitType → ℤ) → ℤ,UnknownType()),BooleanType → (UnitTy
                                                  //| pe → ℤ) → (UnitType → ℤ) → ℤ))
                                                  //|                  x5lit
                                                  //|                  (Pair(ProductType((UnitType → ℤ) → (UnitType → ℤ
                                                  //| ) → ℤ,UnknownType()), BooleanType → (UnitType → ℤ) → (UnitType 
                                                  //| → ℤ) → ℤ)
                                                  //|                     x6lit
                                                  //|                     x7lit))))))
                                                  //|   a_12
                                                  //|   aTot_12
                                                  //|   a_11
                                                  //|   aTot_10
                                                  //|   a_9
                                                  //|   aTot_8
                                                  //|   a_7"
  show(addCaches(test1))                          //> res2: String = "
                                                  //| id =
                                                  //|   λx.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair((ℤ → ℤ) → ℤ → ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       x
                                                  //|       UnitTerm;
                                                  //| id_i =
                                                  //|   λx.
                                                  //|     (λx1lit.
                                                  //|      λx2lit.
                                                  //|        Pair(ℤ, UnitType)
                                                  //|          x1lit
                                                  //|          x2lit)
                                                  //|       x
                                                  //|       UnitTerm;
                                                  //| apply =
                                                  //|   λf.
                                                  //|     Pair(ℤ → ProductType(ℤ,ProductType(ℤ,UnknownType())), UnitType)
                                                  //| 
                                                  //|       (λx.
                                                  //|          aTot_13 =
                                                  //|            f
                                                  //|              x;
                                                  //|          a_13 =
                                                  //|            Proj1(ℤ, UnknownType())
                                                  //|              aTot_13;
                                                  //|          (λx1lit.
                                                  //|           λx2lit.
                                                  //|             Pair(ℤ, ProductType(ℤ,UnknownType()))
                                                  //|               x1lit
                                                  //|               x2lit)
                                                  //|            a_13
                                                  //|            aTot_13)
                                                  //|       UnitTerm;
                                                  //| aTot_14 =
                                                  //|   id
                                                  //|     apply;
                                                  //| a_14 =
                                                  //|   Proj1((ℤ → ℤ) → ℤ → ℤ, UnknownType())
                                                  //|     aTot_14;
                                                  //| aTot_15 =
                                                  //|   a_14
                                                  //|     id_i;
                                                  //| a_15 =
                                                  //|   Proj1(ℤ → ℤ, UnknownType())
                                                  //|     aTot_15;
                                                  //| aTot_16 =
                                                  //|   id_i
                                                  //|     LiteralInt(3);
                                                  //| a_16 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_16;
                                                  //| aTot_17 =
                                                  //|   a_15
                                                  //|     a_16;
                                                  //| a_17 =
                                                  //|   Proj1(ℤ, UnknownType())
                                                  //|     aTot_17;
                                                  //| (λx1lit.
                                                  //|  λx2lit.
                                                  //|  λx3lit.
                                                  //|  λx4lit.
                                                  //|  λx5lit.
                                                  //|  λx6lit.
                                                  //|  λx7lit.
                                                  //|  λx8lit.
                                                  //|    Pair(ℤ, ProductType(ProductType(ℤ,UnknownType()),ProductType(Product
                                                  //| Type(ℤ,UnknownType()),ProductType(ProductType(ℤ → ℤ,UnknownType()),
                                                  //| ProductType(ProductType((ℤ → ℤ) → ℤ → ℤ,UnknownType()),Produc
                                                  //| tType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) 
                                                  //| → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))))))
                                                  //|      x1lit
                                                  //|      (Pair(ProductType(ℤ,UnknownType()), ProductType(ProductType(ℤ,Unkn
                                                  //| ownType()),ProductType(ProductType(ℤ → ℤ,UnknownType()),ProductType(P
                                                  //| roductType((ℤ → ℤ) → ℤ → ℤ,UnknownType()),ProductType((ℤ �
                                                  //| � ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → �1505 ��) → (ℤ → ℤ) → ℤ → ℤ))))))
                                                  //|         x2lit
                                                  //|         (Pair(ProductType(ℤ,UnknownType()), ProductType(ProductType(ℤ �1505 �� ℤ,UnknownType()),ProductType(ProductType((ℤ → ℤ) → ℤ → ℤ
                                                  //| ,UnknownType()),ProductType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ �1505 �� ℤ,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))
                                                  //| ))
                                                  //|            x3lit
                                                  //|            (Pair(ProductType(ℤ → ℤ,UnknownType()), ProductType(Produc
                                                  //| tType((ℤ → ℤ) → ℤ → ℤ,UnknownType()),ProductType((ℤ → ℤ
                                                  //| ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ → ℤ) → ℤ → ℤ) �1505 �� (ℤ → ℤ) → ℤ → ℤ))))
                                                  //|               x4lit
                                                  //|               (Pair(ProductType((ℤ → ℤ) → ℤ → ℤ,UnknownType()
                                                  //| ), ProductType((ℤ → ℤ) → ℤ → ℤ,ProductType(ℤ → ℤ,((ℤ 
                                                  //| → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ)))
                                                  //|                  x5lit
                                                  //|                  (Pair((ℤ → ℤ) → ℤ → ℤ, ProductType(ℤ → �1505 ��,((ℤ → ℤ) → ℤ → ℤ) → (ℤ → ℤ) → ℤ → ℤ))
                                                  //|                     x6lit
                                                  //|                     (Pair(ℤ → ℤ, ((ℤ → ℤ) → ℤ → ℤ) → 
                                                  //| (ℤ → ℤ) → ℤ → ℤ)
                                                  //|                        x7lit
                                                  //|                        x8lit)))))))
                                                  //|   a_17
                                                  //|   aTot_17
                                                  //|   aTot_16
                                                  //|   aTot_15
                                                  //|   aTot_14
                                                  //|   apply
                                                  //|   id_i
                                                  //|   id"
                                                  /*
  val bindings = createBindings()
  val normalT = aNormalizeAddCaches(t, bindings)
  val withAdaptedCallers = adaptCallers(normalT)
  val adaptedBindings = bindings.bindings.toList map {
	  case (t, v) => (adaptCallers(t), v)
  }
  adaptedBindings map {
   case (a, b) => s"\n$b:\n ${pretty(a)}\n"
  } mkString ""
                                                  */
}