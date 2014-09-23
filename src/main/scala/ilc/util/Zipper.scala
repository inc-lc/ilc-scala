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
  def getChildren(tree: Tree): Seq[Subtree] = Seq.empty

  /** inside-out context. */
  trait Context
  {
    /** go-up for paths */
    def parent: Context

    /** copy self with new parent
      * boilerplate obligation of subclasses
      */
    def updateParent(newParent: Context): Context

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
    def prepend(superpath: Context): Context =
      this updateParent (parent prepend superpath)

    def holePosition: Int = 0
  }

  /**
    * This represents the empty path, leading to the top of the represented tree.
    */
  case object Top extends Context {
    override def parent: Context =
      throw ParentOfTopException

    override def updateParent(newParent: Context): Context =
      throw UpdateParentOfTopException

    override def instantiate(subtree: Tree): Tree =
      throw InstantiateOfTopException

    override def plugin(subtree: Tree): Tree = subtree

    override def prepend(superpath: Context): Context = superpath
  }

  /**
    * A location is a pair of a Tree contained in the location, together
    * with a Context, that is a tree with a hole.
    */
  case class Subtree(subtree: Tree, pathToRoot: Context) {
    def root: Tree = pathToRoot plugin subtree

    def isRoot: Boolean = pathToRoot == Top

    def parent: Subtree =
      Subtree(pathToRoot instantiate subtree, pathToRoot.parent)

    def siblingOrdinalPosition: Int = pathToRoot.holePosition

    def children: Seq[Subtree] =
      getChildren(subtree) map {
        case Subtree(childTerm, pathToThis) =>
          Subtree(childTerm, pathToThis prepend this.pathToRoot)
      }
  }

  object Subtree {
    def ofRoot(root: Tree): Subtree =
      Subtree(root, Top)
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
