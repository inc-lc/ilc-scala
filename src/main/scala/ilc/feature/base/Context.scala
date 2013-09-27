package ilc
package feature
package base

import ilc.util.Zipper

/** Term contexts: C[·] */

trait Context extends Syntax with Zipper {
  type Tree = Term
  type Context = Path
  type Subterm = Location

  implicit class SubtermOps(subterm: Subterm) {
    def toTerm: Term = subterm.subtree
  }
}
