package ilc
package feature
package cbpv

object CBPVTest extends TypeConversions {
  //println("Welcome to the Scala worksheet")
  cbnTypeToCBPV(SumType(UnitType, UnitType))      //> res0: ilc.feature.cbpv.CBPVTest.CompType = FProducerCT(SumVT(UThunkVT(FProdu
                                                  //| cerCT(UnitVT)),UThunkVT(FProducerCT(UnitVT))))
  compTypeToType(cbnTypeToCBPV(BooleanType))      //> res1: ilc.feature.cbpv.CBPVTest.Type = F(( (1) + (1) ))
  compTypeToType(cbnTypeToCBPV(BooleanType =>: BooleanType))
                                                  //> res2: ilc.feature.cbpv.CBPVTest.Type = U(F(( (1) + (1) ))) -> F(( (1) + (1) 
                                                  //| ))
  compTypeToType(cbnTypeToCBPV(BooleanType =>: BooleanType =>: BooleanType))
                                                  //> res3: ilc.feature.cbpv.CBPVTest.Type = U(F(( (1) + (1) ))) -> U(F(( (1) + (1
                                                  //| ) ))) -> F(( (1) + (1) ))
  valTypeToType(cbvTypeToCBPV(BooleanType =>: BooleanType =>: BooleanType))
                                                  //> res4: ilc.feature.cbpv.CBPVTest.Type = U(( (1) + (1) ) -> F(U(( (1) + (1) ) 
                                                  //| -> F(( (1) + (1) )))))
  compTypeToType(cbnTypeToCBPV(UnitType =>: SumType(UnitType, UnitType)))
                                                  //> res5: ilc.feature.cbpv.CBPVTest.Type = U(F(1)) -> F(( (U(F(1))) + (U(F(1))) 
                                                  //| ))
}
