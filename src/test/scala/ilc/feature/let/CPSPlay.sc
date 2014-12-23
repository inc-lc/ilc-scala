package ilc
package feature
package let

object CPSPlay extends CPSTestingHelper {
  import bacchusSystem._
  verboseShowTerm(normalize(tstA), "a")           //> Raw a term: Abs(Var(y_1,(T12 -> T13) -> T12),Abs(Var(x_2,T12 -> T13),App(Var
                                                  //| (x_2,T12 -> T13),App(Var(y_1,(T12 -> T13) -> T12),Var(x_2,T12 -> T13)))))
                                                  //| Pretty a term:
                                                  //| λy_1.
                                                  //| λx_2.
                                                  //|   x_2
                                                  //|     (y_1
                                                  //|        x_2)
                                                  //| Type: ((T12 -> T13) -> T12) -> (T12 -> T13) -> T13
                                                  //| 
  verboseShowTerm(normalize(tstB), "b")           //> Raw b term: Abs(Var(y_1,(T18 -> T19) -> T18),Abs(Var(x_2,T18 -> T19),App(Var
                                                  //| (x_2,T18 -> T19),App(Var(y_1,(T18 -> T19) -> T18),Abs(Var(z_3,T18),App(Var(x
                                                  //| _2,T18 -> T19),Var(z_3,T18)))))))
                                                  //| Pretty b term:
                                                  //| λy_1.
                                                  //| λx_2.
                                                  //|   x_2
                                                  //|     (y_1
                                                  //|        (λz_3.
                                                  //|           x_2
                                                  //|             z_3))
                                                  //| Type: ((T18 -> T19) -> T18) -> (T18 -> T19) -> T19
                                                  //| 
  val tstAUnt = (toCPSU(tstA))('c ->: 'c)         //> tstAUnt  : ilc.feature.let.CPSPlay.bacchusSystem.UApp = UApp(UAbs(k_1,None,U
                                                  //| App(UVar(k_1),UAbs(y,None,UAbs(k_2,None,UApp(UVar(k_2),UAbs(x,None,UAbs(k_3,
                                                  //| None,UApp(UAbs(k_6,None,UApp(UVar(k_6),UVar(x))),UAbs(a_4,None,UApp(UAbs(k_7
                                                  //| ,None,UApp(UAbs(k_10,None,UApp(UVar(k_10),UVar(y))),UAbs(a_8,None,UApp(UAbs(
                                                  //| k_11,None,UApp(UVar(k_11),UVar(x))),UAbs(b_9,None,UApp(UApp(UVar(a_8),UVar(b
                                                  //| _9)),UVar(k_7))))))),UAbs(b_5,None,UApp(UApp(UVar(a_4),UVar(b_5)),UVar(k_3))
                                                  //| ))))))))))),UAbs(c,None,UVar(c)))
  val tstBUnt = (toCPSU(tstB))('c ->: 'c)         //> tstBUnt  : ilc.feature.let.CPSPlay.bacchusSystem.UApp = UApp(UAbs(k_12,None,
                                                  //| UApp(UVar(k_12),UAbs(y,None,UAbs(k_13,None,UApp(UVar(k_13),UAbs(x,None,UAbs(
                                                  //| k_14,None,UApp(UAbs(k_17,None,UApp(UVar(k_17),UVar(x))),UAbs(a_15,None,UApp(
                                                  //| UAbs(k_18,None,UApp(UAbs(k_21,None,UApp(UVar(k_21),UVar(y))),UAbs(a_19,None,
                                                  //| UApp(UAbs(k_22,None,UApp(UVar(k_22),UAbs(z,None,UAbs(k_23,None,UApp(UAbs(k_2
                                                  //| 6,None,UApp(UVar(k_26),UVar(x))),UAbs(a_24,None,UApp(UAbs(k_27,None,UApp(UVa
                                                  //| r(k_27),UVar(z))),UAbs(b_25,None,UApp(UApp(UVar(a_24),UVar(b_25)),UVar(k_23)
                                                  //| ))))))))),UAbs(b_20,None,UApp(UApp(UVar(a_19),UVar(b_20)),UVar(k_18))))))),U
                                                  //| Abs(b_16,None,UApp(UApp(UVar(a_15),UVar(b_16)),UVar(k_14))))))))))))),UAbs(c
                                                  //| ,None,UVar(c)))
  verboseShowTerm(normalize(tstAUnt), "a I")      //> Raw a I term: Abs(Var(y_1,(T39 -> T24 -> T41) -> (T39 -> T41) -> T37),Abs(Va
                                                  //| r(k_2,((T39 -> T24 -> T41) -> T24 -> T37) -> T44),App(Var(k_2,((T39 -> T24 -
                                                  //| > T41) -> T24 -> T37) -> T44),Abs(Var(x_3,T39 -> T24 -> T41),Abs(Var(k_4,T24
                                                  //| ),App(App(Var(y_1,(T39 -> T24 -> T41) -> (T39 -> T41) -> T37),Var(x_3,T39 ->
                                                  //|  T24 -> T41)),Abs(Var(b_5,T39),App(App(Var(x_3,T39 -> T24 -> T41),Var(b_5,T3
                                                  //| 9)),Var(k_4,T24)))))))))
                                                  //| Pretty a I term:
                                                  //| λy_1.
                                                  //| λk_2.
                                                  //|   k_2
                                                  //|     (λx_3.
                                                  //|      λk_4.
                                                  //|        y_1
                                                  //|          x_3
                                                  //|          (λb_5.
                                                  //|             x_3
                                                  //|               b_5
                                                  //|               k_4))
                                                  //| Type: ((T39 -> T24 -> T41) -> (T39 -> T41) -> T37) -> (((T39 -> T24 -> T41) 
                                                  //| -> T24 -> T37) -> T44) -> T44
                                                  //| 
  verboseShowTerm(normalize(tstBUnt), "b I")      //> Raw b I term: Abs(Var(y_1,(T68 -> T62 -> T71) -> (T68 -> T71) -> T76),Abs(Va
                                                  //| r(k_2,((T68 -> T62 -> T71) -> T62 -> T76) -> T84),App(Var(k_2,((T68 -> T62 -
                                                  //| > T71) -> T62 -> T76) -> T84),Abs(Var(x_3,T68 -> T62 -> T71),Abs(Var(k_4,T62
                                                  //| ),App(App(Var(y_1,(T68 -> T62 -> T71) -> (T68 -> T71) -> T76),Abs(Var(z_5,T6
                                                  //| 8),Abs(Var(k_6,T62),App(App(Var(x_3,T68 -> T62 -> T71),Var(z_5,T68)),Var(k_6
                                                  //| ,T62))))),Abs(Var(b_7,T68),App(App(Var(x_3,T68 -> T62 -> T71),Var(b_7,T68)),
                                                  //| Var(k_4,T62)))))))))
                                                  //| Pretty b I term:
                                                  //| λy_1.
                                                  //| λk_2.
                                                  //|   k_2
                                                  //|     (λx_3.
                                                  //|      λk_4.
                                                  //|        y_1
                                                  //|          (λz_5.
                                                  //|           λk_6.
                                                  //|             x_3
                                                  //|               z_5
                                                  //|               k_6)
                                                  //|          (λb_7.
                                                  //|             x_3
                                                  //|               b_7
                                                  //|               k_4))
                                                  //| Type: ((T68 -> T62 -> T71) -> (T68 -> T71) -> T76) -> (((T68 -> T62 -> T71) 
                                                  //| -> T62 -> T76) -> T84) -> T84
                                                  //| 
  
  //          x_3
  // in the above term becomes
  //          (λz_5.
  //           λk_6.
  //             x_3
  //               z_5
  //               k_6)
  // because it gets eta-expanded and CPS-transformed
}