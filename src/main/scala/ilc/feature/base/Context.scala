package ilc
package feature
package base

import ilc.util.Zipper

/** Term contexts: C[·] */

trait Context extends Syntax with Zipper {
  type Tree = Term

  implicit class SubtreeOps(subtree: Subtree) {
    def toTerm: Term = subtree.subtree
  }
}
