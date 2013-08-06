package ilc
package feature

/**
 * Commonality of calculi with diff/apply terms
 */

trait DiffAndApply { self: functions.Syntax =>
  val diffTerm: Term
  val applyTerm: Term
}
