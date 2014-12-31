package ilc
package feature
package let

object CPSPlay extends CPSTestingHelper {
  import bacchusSystem._
  //Altered from an example in Andrew Kennedy's paper "Compiling with Continuations, Continued".
  val tst0U = ('x ->: 'j)('f('y))                 //> tst0U  : ilc.feature.let.CPSPlay.bacchusSystem.UApp = UApp(UAbs(x,None,UVar(
                                                  //| j)),UApp(UVar(f),UVar(y)))
  val tst0: Term = tst0U                          //> tst0  : ilc.feature.let.CPSPlay.bacchusSystem.Term = App(Abs(Var(x,T24),Var(
                                                  //| j,T25)),App(Var(f,T23 -> T24),Var(y,T23)))
  val tst1: Term = 'j ->: 'f ->: 'y ->: tst0U     //> tst1  : ilc.feature.let.CPSPlay.bacchusSystem.Term = Abs(Var(j,T31),Abs(Var(
                                                  //| f,T28 -> T30),Abs(Var(y,T28),App(Abs(Var(x,T30),Var(j,T31)),App(Var(f,T28 ->
                                                  //|  T30),Var(y,T28))))))
  verboseShowTerm(toCPSOnePass(tst0), "")         //> Raw  term: Abs(Var(k_1,T25 -> AnswerT),App(App(Var(f,T23 -> (T24 -> AnswerT)
                                                  //|  -> AnswerT),Var(y,T23)),Abs(Var(a_3,T24),App(App(Abs(Var(x,T24),Abs(Var(k_2
                                                  //| ,T25 -> AnswerT),App(Var(k_2,T25 -> AnswerT),Var(j,T25)))),Var(a_3,T24)),Var
                                                  //| (k_1,T25 -> AnswerT)))))
                                                  //| Pretty  term:
                                                  //| λk_1.
                                                  //|   f
                                                  //|     y
                                                  //|     (λa_3.
                                                  //|        (λx.
                                                  //|         λk_2.
                                                  //|           k_2
                                                  //|             j)
                                                  //|          a_3
                                                  //|          k_1)
                                                  //| Type: (T25 -> AnswerT) -> AnswerT
                                                  //| 
  verboseShowTerm(toCPSOnePass(tst1), "")         //> Raw  term: Abs(Var(k_4,(T31 -> (((T28 -> (T30 -> AnswerT) -> AnswerT) -> ((T
                                                  //| 28 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> An
                                                  //| swerT) -> AnswerT),App(Var(k_4,(T31 -> (((T28 -> (T30 -> AnswerT) -> AnswerT
                                                  //| ) -> ((T28 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> Answer
                                                  //| T) -> AnswerT) -> AnswerT),Abs(Var(j,T31),Abs(Var(k_5,((T28 -> (T30 -> Answe
                                                  //| rT) -> AnswerT) -> ((T28 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> Answ
                                                  //| erT) -> AnswerT),App(Var(k_5,((T28 -> (T30 -> AnswerT) -> AnswerT) -> ((T28 
                                                  //| -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT),Abs(Var(
                                                  //| f,T28 -> (T30 -> AnswerT) -> AnswerT),Abs(Var(k_6,(T28 -> (T31 -> AnswerT) -
                                                  //| > AnswerT) -> AnswerT),App(Var(k_6,(T28 -> (T31 -> AnswerT) -> AnswerT) -> A
                                                  //| nswerT),Abs(Var(y,T28),Abs(Var(k_7,T31 -> AnswerT),App(App(Var(f,T28 -> (T30
                                                  //|  -> AnswerT) -> AnswerT),Var(y,T28)),Abs(Var(a_9,T30),App(App(Abs(Var(x,T30)
                                                  //| ,Abs(Var(k_8,T31 -> AnswerT),App(Var(k_8,T31 -> AnswerT),Var(j,T31)))),Var(a
                                                  //| _9,T30)),Var(k_7,T31 -> AnswerT))))))))))))))
                                                  //| Pretty  term:
                                                  //| λk_4.
                                                  //|   k_4
                                                  //|     (λj.
                                                  //|      λk_5.
                                                  //|        k_5
                                                  //|          (λf.
                                                  //|           λk_6.
                                                  //|             k_6
                                                  //|               (λy.
                                                  //|                λk_7.
                                                  //|                  f
                                                  //|                    y
                                                  //|                    (λa_9.
                                                  //|                       (λx.
                                                  //|                        λk_8.
                                                  //|                          k_8
                                                  //|                            j)
                                                  //|                         a_9
                                                  //|                         k_7))))
                                                  //| Type: ((T31 -> (((T28 -> (T30 -> AnswerT) -> AnswerT) -> ((T28 -> (T31 -> An
                                                  //| swerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> Answer
                                                  //| T) -> AnswerT
                                                  //| 
  verboseShowTerm(normalize(toCPSUntypedOnePass(tst1)), "")
                                                  //> Raw  term: Abs(Var(k_1,(T31 -> (((T28 -> (T30 -> AnswerT) -> AnswerT) -> ((T
                                                  //| 28 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> An
                                                  //| swerT) -> AnswerT),App(Var(k_1,(T31 -> (((T28 -> (T30 -> AnswerT) -> AnswerT
                                                  //| ) -> ((T28 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> Answer
                                                  //| T) -> AnswerT) -> AnswerT),Abs(Var(j_2,T31),Abs(Var(k_3,((T28 -> (T30 -> Ans
                                                  //| werT) -> AnswerT) -> ((T28 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> An
                                                  //| swerT) -> AnswerT),App(Var(k_3,((T28 -> (T30 -> AnswerT) -> AnswerT) -> ((T2
                                                  //| 8 -> (T31 -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT),Abs(Va
                                                  //| r(f_4,T28 -> (T30 -> AnswerT) -> AnswerT),Abs(Var(k_5,(T28 -> (T31 -> Answer
                                                  //| T) -> AnswerT) -> AnswerT),App(Var(k_5,(T28 -> (T31 -> AnswerT) -> AnswerT) 
                                                  //| -> AnswerT),Abs(Var(y_6,T28),Abs(Var(k_7,T31 -> AnswerT),App(App(Var(f_4,T28
                                                  //|  -> (T30 -> AnswerT) -> AnswerT),Var(y_6,T28)),Abs(Var(a_8,T30),App(Var(k_7,
                                                  //| T31 -> AnswerT),Var(j_2,T31))))))))))))))
                                                  //| Pretty  term:
                                                  //| λk_1.
                                                  //|   k_1
                                                  //|     (λj_2.
                                                  //|      λk_3.
                                                  //|        k_3
                                                  //|          (λf_4.
                                                  //|           λk_5.
                                                  //|             k_5
                                                  //|               (λy_6.
                                                  //|                λk_7.
                                                  //|                  f_4
                                                  //|                    y_6
                                                  //|                    (λa_8.
                                                  //|                       k_7
                                                  //|                         j_2))))
                                                  //| Type: ((T31 -> (((T28 -> (T30 -> AnswerT) -> AnswerT) -> ((T28 -> (T31 -> An
                                                  //| swerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> AnswerT) -> Answer
                                                  //| T) -> AnswerT
                                                  //| 
  //verboseShowTerm(normalize(tstA), "a")
  //verboseShowTerm(normalize(tstB), "b")
  //val tstAUnt = (toCPSU(tstA))('c ->: 'c)
  //val tstBUnt = (toCPSU(tstB))('c ->: 'c)
  //verboseShowTerm(normalize(tstAUnt), "a I")
  //verboseShowTerm(normalize(tstBUnt), "b I")
  
  //          x_3
  // in the above term becomes
  //          (λz_5.
  //           λk_6.
  //             x_3
  //               z_5
  //               k_6)
  // because it gets eta-expanded and CPS-transformed
}