package ilc
package language
package gcc

import feature._
import scala.language.implicitConversions
import Predef.{any2stringadd => _, _}

/**
 * TODO add later to api:
 *
 * isWallLeft
 * lookAndCheck(s, direction, material)
 *
 * Two things needed in general
 *
 * 1. query the environment
 * 2. actions: moving...
 */
trait LambdaManApi extends SyntaxSugar with Math with Collection with Pathfinding {

  lazy val all = collectionApi ++ worldApi ++ enumApi ++ characterApi ++ directionApi ++ math ++ points ++ pathfinding

  val Loc: Type = (int, int)
  val Dir: Type = int
  val Character: Type = (int, Loc, Dir, int, int)
  val Ghost: Type = (int, Loc, Dir)
  val Item: Type = int
  val WorldMap: Type = ListType(ListType(Item))
  val WorldState: Type = (WorldMap, Character, ListType(Ghost), int)

  object move {
    val up    = 0
    val right = 1
    val down  = 2
    val left  = 3

    val first = up
    val last = left
  }

  /**
   * The state of the world is encoded as follows:
   *
   * A 4-tuple consisting of
   *
   * 1. The map;
   * 2. the status of Lambda-Man;
   * 3. the status of all the ghosts;
   * 4. the status of fruit at the fruit location.
   *
   * The map is encoded as a list of lists (row-major) representing the 2-d
   * grid. An enumeration represents the contents of each grid cell:
   */
  val worldApi = Seq(
    fun('world_map)('world % WorldState) { 'world.at(0, 4) },
    fun('world_lambdaStatus)('world % WorldState) { 'world at(1, 4) },
    fun('world_itemAt)('world % WorldState, 'x % int, 'y % int) { elemAt(elemAt('world at(0, 4), 'y), 'x) },

    /**
     * Returns a list of ghosts
     *
     * We pad it to a 5-tuple in order to be compatible with lambda man
     */
    fun('world_ghostsStatus)('world % WorldState) {
      map('world at(2, 4), lam('el) {
        'el.bind('vit, 'loc, 'dir) {
          tuple('vit, 'loc, 'dir, 0, 0)
        }
      })
    },

    /**
     * The status of the fruit is a number which is a countdown to the expiry of
     * the current fruit, if any.
     * - 0: no fruit present;
     * - n > 0: fruit present: the number of game ticks remaining while the
     *          fruit will will be present.
     */
    fun('world_fruitStatus)('world % WorldState) { 'world at(3, 4) }
  )

  /**
   * - 0: Wall (`#`)
   * - 1: Empty (`<space>`)
   * - 2: Pill
   * - 3: Power pill
   * - 4: Fruit location
   * - 5: Lambda-Man starting position
   * - 6: Ghost starting position
   */
  val enumApi = Seq(
    fun('isWall)('obj % Item) { 'obj === 0 },
    fun('isEmptyField)('obj % Item) { 'obj === 1 },
    fun('isPill)('obj % Item) { 'obj === 2 },
    fun('isPowerPill)('obj % Item) { 'obj === 3 },
    fun('isFruit)('obj % Item) { 'obj === 4 },
    fun('isLambdaStart)('obj % Item) { 'obj === 5 },
    fun('isGhostStart)('obj % Item) { 'obj === 6 }
  )

  /**
   * Lambda Man
   * ----------
   * The Lambda-Man status is a 5-tuple consisting of:
   *
   * 1. Lambda-Man's vitality;
   * 2. Lambda-Man's current location, as an (x,y) pair;
   * 3. Lambda-Man's current direction;
   * 4. Lambda-Man's remaining number of lives;
   * 5. Lambda-Man's current score.
   *
   * For ghosts:
   * -----------
   * The status for each ghost is a 5-tuple
   * (to abstract over lambda and ghosts at the same time) consisting of
   *
   * 1. the ghost's vitality
   * 2. the ghost's current location, as an (x,y) pair
   * 3. the ghost's current direction
   * 4. not defined!
   * 5. not defined!
   *
   * Lambda-Man's vitality is a number which is a countdown to the expiry of
   * the active power pill, if any. It is 0 when no power pill is active.
   *   - 0: standard mode;
   *   - n > 0: power pill mode: the number of game ticks remaining while the
   *            power pill will will be active
   */
  val characterApi = Seq(
    fun('vitality)('char % Character) { 'char at(0, 5) },
    fun('location)('char % Character) { 'char at(1, 5) },
    fun('direction)('char % Character) { 'char at(2, 5) },

    /**
     * Only works for lambdas!
     */
    fun('lives)('char % Character) { 'char at(3, 5) },
    fun('score)('char % Character) { 'char at(4, 5) },

    // for ghosts:
    fun('isAfraid)('ghost % Character) { 'vitality('ghost) === 1 },
    fun('isInvisible)('ghost % Character) { 'vitality('ghost) === 2 },

    // for lambda man:
    fun('powerLeft)('lambdaMan % Character) { 'lambdaMan at(0, 5) },
    fun('isInPowerMode)('lambdaMan % Character) { 'powerLeft('lambdaMan) =!= 0 }
  )

  /**
   *  The Ghosts' and Lambda-Man's direction is an enumeration:
   * - 0: up;
   * - 1: right;
   * - 2: down;
   * - 3: left.
   */
  val directionApi = Seq(
    fun('isUp)('obj % Dir) { 'obj === move.up },
    fun('isRight)('obj % Dir) { 'obj === move.right },
    fun('isDown)('obj % Dir) { 'obj === move.down },
    fun('isLeft)('obj % Dir) { 'obj === move.left },
    fun('nextDir)('obj % Dir) { letS('next := 'obj + 1) { if_ ('next > move.last) {move.first} else_ 'next } }
  )

}

trait Collection extends SyntaxSugar { outer =>

  def elemAt(list: UT, i: UT) =
    letrec {
        fun('goElemAt)('l, 'i % int) {
          if_('i === 0) {
            'l.head
          } else_ {
            'goElemAt('l.tail, 'i - 1)
          }
        }
      }("elemAtBody", 'goElemAt(list, i))

  def foldRight(list: UT, z: UT, f: UT) =
    letrec {
      fun('go)('l) {
        if_('l.isEmpty) {
          z
        } else_ {
          f('l.head, 'go('l.tail))
        }
      }
    }("foldRightBody", 'go(list))

  def map(list: UT, f: UT) =
    foldRight(list, empty, lam('head, 'tail) {
      f('head) ::: 'tail
    })

  def size(list: UT) =
    foldRight(list, 0, lam('_, 'n) { 'n + 1 })

  //This contains only monomorphic functions, all polymorphic ones must be macros as above!
  val collectionApi = Seq(
    fun('all)('list % ListType(BooleanType)) {
      foldRight('list, true, lam('b1, 'b2) { 'b1 and 'b2 })
    },
    fun('any)('list % ListType(BooleanType)) {
      foldRight('list, false, lam('b1, 'b2) { 'b1 or 'b2 })
    }
  )

  implicit class ListOpsExt[T <% UT](list: T) {
    def size: UT = outer.size(list)

    def contains(el: UT, comp: UT /* (T, T) =>: bool */)
      = letrec(
        'go := lam('list, 'el, 'comp) {
          if_('list.isEmpty) {
            false
          }. else_if('comp('list.head, 'el)) {
            true
          } else_ {
            'go('list.tail, 'el, 'comp)
          }
        }
      )("containsBody", 'go(list, el, comp))

    def filter[T](comp: UT /* T =>: bool */)
      = foldRight(list, empty, lam('el, 'acc) {
        if_(not(comp('el))) {
          'el ::: 'acc
        } else_ {
          'acc
        }
      })
    /*
     * zip
     */
    /*def zip(other: UT)
      = letrec(
          'go := lam('l1, 'l2) {
          }
        )("zipMain", 'go('list, 'other))*/

    def search(comp: UT /* T =>: bool */)
      = foldRight(list, (empty, 0), lam('el, 'accPair) {
        'acc.bind('acc, 'pos) {
          (if_(not(comp('el))) {
            ('el, 'pos) ::: 'acc
          } else_ {
            'acc
          }, 'pos + 1)
        }
      }).first
  }
}

// requires Math and Points
trait Pathfinding extends SyntaxSugar with Points with Collection { self: LambdaManApi =>

  val ParentMap = ListType(ListType(Point))
  val GMap      = ListType(ListType(int))
  val Queue     = ListType(Point)

  // used in loop body
  val LocalState = (ParentMap, Queue, GMap)

  val pathfinding = Seq(

    fun('manhattan)('from % Point, 'to % Point) {
      ('abs('from.x - 'to.x) + 'abs('from.y - 'to.y))
    },

    /**
     * Computes the path as list, starting with the next move
     * If no path could be found the list is empty
     */
    fun('computePath)('from % Point, 'to % Point, 'map % GameMap) {
      letrec (
        'parents := 'computeParents('from, 'to, 'map),
        'computePathGo := lam('curr, 'acc) {
          if_('pointEq('curr, 'from)) {
            'from ::: 'acc
          } else_{
            let('next, 'parents atPos 'curr) {
              'computePathGo('next, 'next ::: 'acc)
            }
          }
        }
      )("computePathBody", if_('parents.isEmpty) { empty } else_ { 'computePathGo('to, empty).filter(lam('el){ 'pointEq('el, 'from) }) })

    },

    fun('computeParents)('start % Point, 'target % Point, 'map % GameMap) {
      letS(
        // parameters to control the algorithm
        'dg := 1,
        'dh := 1,

        // distance measures
        'H := lam('a % Point) { 'dh * 'manhattan('a, 'target) },
        'G := lam('a % Point, 'gMap % GMap) {
          let('value, 'gMap atPos 'a) {
            if_('value === -1) { 'dg * 'manhattan('a, 'start) } else_{ 'value }
          }
        },
        'F := lam('a % Point, 'gMap % GMap) { 'G('a, 'gMap) + 'H('a) },

        // the last cells of the map
        'limit := ('map.head.size - 1, 'map.size - 1),
        'inRange := lam('p % Point) { 'p.x >= 0 and 'p.y >= 0 and 'p.x <= 'limit.x and 'p.y <= 'limit.y },

        'isObstacle := lam('a % Point) { ('map atPos 'a) === 0 },

        'parentsInit := createMap('map.width, 'map.height, (-1, -1)) ofType ParentMap,
        'gMapInit    := createMap('map.width, 'map.height, -1) ofType GMap
      ) {
        letrec {
          fun('loop)('curCell % Point, 'openList % Queue, 'closedList % Queue, 'parents % ParentMap, 'gMap % GMap) {
            letS(
              'Ncell := 'curCell |+| ( 0, -1),
              'Scell := 'curCell |+| ( 0,  1),
              'Ecell := 'curCell |+| ( 1,  0),
              'Wcell := 'curCell |+| (-1,  0),
              'cells := list('Ncell, 'Scell, 'Ecell, 'Wcell),

              'alreadyHandled := lam('p % Point) { 'closedList contains ('p, 'pointEq) },
              'shouldConsider := lam('p % Point) { not('isObstacle('p) or 'alreadyHandled('p)) },

               /**
                * Updates the parents, openList and gMap for the given cell
                *
                * Returns the updated localState
                */
              'checkCell := lam('p % Point, 'localState % LocalState) {

                'localState.bind('parents, 'openList, 'gMap) {
                  if_('inRange('p) and 'shouldConsider('p)) {
                    if_(not('openList contains ('p, 'pointEq))) {
                      tuple('parents.update(Point)('p, 'curCell), 'p ::: 'openList, 'gMap)
                    }. else_if (('G('curCell, 'gMap) + 'dg) < 'G('p, 'gMap)) {
                      // update parents and gmap
                      tuple('parents.update(Point)('p, 'curCell), 'openList, 'gMap.update(int)('p, 'G('curCell, 'gMap) + 'dg))
                    } else_ {
                      tuple('parents, 'openList, 'gMap)
                    }
                  } else_ {
                    tuple('parents, 'openList, 'gMap) // did not change anything
                  }
                }
              },

              /**
               * In a list of points searches the minimum comparing the F values
               * The provided list must not be empty!
               *
               * Returns the found minimum point
               */
              'findCellWithSmallestF := lam('list % Queue, 'gMap % GMap) {
                  foldRight('list, 'list.head, lam('el, 'curMax) {
                    if_('F('el, 'gMap) < 'F('curMax, 'gMap)) { 'el } else_ { 'curMax }
                 })
               },

               'nextState := foldRight('cells, tuple('parents, 'openList, 'gMap), 'checkCell)
            ) {
              'nextState.bind('parents, 'openList, 'gMap) {
                if_('openList.isEmpty) {
                  empty //return error! nothing found...
                } else_{
                  letS(
                    'nextCell       := 'findCellWithSmallestF('openList, 'gMap),
                    'nextOpenList   := 'openList.filter(lam('el){ 'pointEq('el, 'nextCell) }),
                    'nextClosedList := 'nextCell ::: 'closedList
                  ) {
                    if_('pointEq('nextCell, 'target)) {
                      'parents
                    } else_ {
                      'loop('nextCell, 'nextOpenList, 'nextClosedList, 'parents, 'gMap)
                    }
                  }
                }
              }
            }
          }
        }("pathfindingBody", 'loop('start, empty, 'start ::: empty, 'parentsInit, 'gMapInit))
      }
    }
  )
}

trait Math extends SyntaxSugar {

  val math = Seq(
    fun('mod)('x % int, 'y % int) { 'x - ('x / 'y * 'y) },
    fun('abs)('x % int) { if_('x < 0) { 'x * (-1) } else_ { 'x } }
  )

}


trait Points extends SyntaxSugar with Collection { outer: LambdaManApi =>

  val Point: Type = (int, int)
  val GameMap: Type = ListType(ListType(int))

  // macro for accessing points
  implicit class PointOps[T <% UT](pt: T) {
    def x: UT = (pt ofType Point).first
    def y: UT = (pt ofType Point).second

    // please note that we do not use let here!
    def |+|(other: UT) =
      ((pt ofType Point).x + (other ofType Point).x, pt.y + other.y)

    def |-|(other: UT) =
      ((pt ofType Point).x - (other ofType Point).x, pt.y - other.y)

    def moveTo(other: UT) = 'vectorToMove(other |-| pt)
  }

  implicit class MapOps[T <% UT](list: T) {
    def atPos(p: UT) =
      letS('l := list, 'x := p.x, 'y := p.y) { elemAt(elemAt('l, 'y), 'x) }

    def update(ET: Type)(pt: UT, value: UT) = {
      val MapT = ListType(ListType(ET))
      val ListT = ListType(ET)
      letrec(
        'value := value,
        'x     := pt.x,
        'y     := pt.y,

        'findColumn := lam('i, 'cols) {
          if_('cols.isEmpty) {
            debug(1333) ~: empty
          }. else_if ('i === 'y) {
            'findCell(0, 'cols.head) ::: 'cols.tail
          } else_{
            'cols.head ::: 'findColumn('i + 1, 'cols.tail)
          }
        },

        'findCell := lam('j, 'cells) {
          if_('cells.isEmpty) {
            debug(1334) ~: empty
          }. else_if ('j === 'x) {
            'value ::: 'cells.tail
          } else_{
            'cells.head ::: 'findCell('j + 1, 'cells.tail)
          }
        }
      )("updateBody", 'findColumn(0, list))
   }

    def width: UT = list.head.size
    def height: UT = list.size
  }

  def createMap(width: UT, height: UT, init: UT): UT =
    let('row, createList(width, init)) { createList(height, 'row) }

  def createList(length: UT, init: UT): UT =
    letrec(
      'go := lam('n, 'init) {
        if_('n === 0) { empty } else_ { 'init ::: 'go('n - 1, 'init) }
      }
    )("createListBody", 'go(length, init))

  val points = Seq(
    fun('pointEq)('p1 % Point, 'p2 % Point) { ('p1.x === 'p2.x) and ('p1.y === 'p2.y) },

    fun('vectorToMove)('v % Point) {
      if_('pointEq('v, (0, -1))) {
        move.up
      }. else_if ('pointEq('v, (1, 0))) {
        move.right
      }. else_if ('pointEq('v, (0, 1))) {
        move.down
      }. else_if ('pointEq('v, (-1, 0))) {
        move.left
      } else_ {
        -1
      }
    }
  )
}