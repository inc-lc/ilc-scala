package ilc
package feature.maybe

/**
 * Values of the Maybe monad encoded with sum and unit
 */

import feature._

trait SumUnitEncoded
extends unit.Evaluation
   with unit.Syntax
   with sums.Evaluation
   with sums.Syntax
{
  trait MaybeValues
  extends FunValues
     with UnitValues
     with SumValues
  {
    object Nothing {
      def apply(): Value = Value.Left(Value.UnitValue)

      def unapply(value: Value): Option[Unit] = value match {
        case Value.Left(Value.UnitValue) => Some(())
        case _ => None
      }
    }

    object Just {
      def apply(value: Value): Value = Value.Right(value)

      def unapply(value: Value): Option[Value] = value match {
        case Value.Right(content) => Some(content)
        case _ => None
      }
    }
  }

  // val Value inherits from FunValues because
  // the supertrait SumValues requires it.
  val Value: MaybeValues

  // helper function to lower Scala's maybe monad to object level
  def lowerMaybe(maybe: Option[Value]): Value = {
    maybe match {
      case None => Value.Nothing()
      case Some(value) => Value.Just(value)
    }
  }

  // helper function to lift an object level Maybe to meta level
  def liftMaybe(maybe: Value): Option[Value] = {
    maybe match {
      case Value.Nothing() => None
      case Value.Just(value) => Some(value)
    }
  }
}
