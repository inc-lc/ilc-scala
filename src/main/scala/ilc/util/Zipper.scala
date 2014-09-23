package ilc
package util

/** Gérard Huet: The Zipper.
  * J. Functional Programming 7(5): 549--554, September 1997.
  */

trait Zipper {

  type Tree

  /** Takes a tree node $tree and returns a sequence of locations representing
    * the children of $tree.
    *
    * The default implementation assumes that a tree node has no children.
    * This method ought to be overriden by `trait Context` inside the relevant
    * feature (see `ilc.feature.functions.Context`).
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

  /** inside-out context. XXX Also called `Context` in some subclasses through a type alias. */
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
      * Top(5) = 5
      * Abs(x, Top)(5) = Abs(x, 5)
      * App(f, Top)(5) = App(f, 5)
      * }}}
      */
    def plugin(subtree: Tree): Tree =
      parent plugin instantiate(subtree)

    /** put this path on the inside of the other path */
    def prepend(superpath: Path): Path =
      this updateParent (parent prepend superpath)

    def holePosition: Int = 0
  }

  /**
    * This represents the empty path, leading to the top of the represented tree.
    */
  case object Top extends Path {
    override def parent: Path =
      throw ParentOfTopException

    override def updateParent(newParent: Path): Path =
      throw UpdateParentOfTopException

    override def instantiate(subtree: Tree): Tree =
      throw InstantiateOfTopException

    override def plugin(subtree: Tree): Tree = subtree

    override def prepend(superpath: Path): Path = superpath
  }

  /**
    * A location is a pair of a Tree contained in the location, together
    * with a Path/Context, that is a tree with a hole.
    */
  case class Location(subtree: Tree, pathToRoot: Path) {
    def root: Tree = pathToRoot plugin subtree

    def isRoot: Boolean = pathToRoot == Top

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
      Location(root, Top)
  }

  class ZipperException(message: String)
  extends Exception(message)

  object ParentOfTopException
  extends ZipperException("parent of Top")

  object UpdateParentOfTopException
  extends ZipperException("can't update the parent of Top" +
    " because it has none")

  object InstantiateOfTopException
  extends ZipperException("can't instantiate at Top" +
    " because we lack contextual information")
}
