package ilc
package util

/** Gérard Huet: The Zipper.
  * J. Functional Programming 7(5): 549--554, September 1997.
  */

trait Zipper {

  type Tree

  /** Assume by default that a tree node has no children.
    *
    * CAUTION:
    * If a subclass forgets to define it for tree nodes
    * with children, then those will not be visited.
    * We take this risk so that we can be lazy and not
    * define `trait Context` for every language feature
    * out there so as to declare that the constants
    * introduced by that feature have no children.
    */
  def getChildren(tree: Tree): Seq[Location] = Seq.empty

  /** inside-out context */
  trait Path
  {
    /** go-up for paths */
    def parent: Path

    /** copy self with new parent
      * boilerplate obligation of subclasses
      */
    def updateParent(newParent: Path): Path

    /** make a tree node appropriate for this location */
    def instantiate(subtree: Tree): Tree

    /** put a subtree at the end of the path, forming a tree
      *
      * Examples (in top-down syntax):
      * {{{
      * Hole(5) = 5
      * Abs(x, Hole)(5) = Abs(x, 5)
      * App(f, Hole)(5) = App(f, 5)
      * }}}
      */
    def plugin(subtree: Tree): Tree =
      parent plugin instantiate(subtree)

    /** put this path on the inside of the other path */
    def prepend(superpath: Path): Path =
      this updateParent (parent prepend superpath)

    def holePosition: Int = 0
  }

  /** Huet's Top
    * "Hole" is more suggestive.
    */
  case object Hole extends Path {
    override def parent: Path =
      throw ParentOfHoleException

    override def updateParent(newParent: Path): Path =
      throw UpdateParentOfHoleException

    override def instantiate(subtree: Tree): Tree =
      throw InstantiateOfHoleException

    override def plugin(subtree: Tree): Tree = subtree

    override def prepend(superpath: Path): Path = superpath
  }

  case class Location(subtree: Tree, pathToRoot: Path) {
    def root: Tree = pathToRoot plugin subtree

    def isRoot: Boolean = pathToRoot == Hole

    def parent: Location =
      Location(pathToRoot instantiate subtree, pathToRoot.parent)

    def siblingOrdinalPosition: Int = pathToRoot.holePosition

    def children: Seq[Location] =
      getChildren(subtree) map {
        case Location(childTerm, pathToThis) =>
          Location(childTerm, pathToThis prepend this.pathToRoot)
      }
  }

  object Location {
    def ofRoot(root: Tree): Location =
      Location(root, Hole)
  }

  class ZipperException(message: String)
  extends Exception(message)

  object ParentOfHoleException
  extends ZipperException("parent of Hole")

  object UpdateParentOfHoleException
  extends ZipperException("can't update the parent of Hole" +
    " because it has none")

  object InstantiateOfHoleException
  extends ZipperException("can't instantiate at Hole" +
    " because we lack contextual information")
}
