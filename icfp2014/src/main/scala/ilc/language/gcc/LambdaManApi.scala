package ilc
package language
package gcc

import feature._
import scala.language.{ implicitConversions, reflectiveCalls }
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

  lazy val Loc: Type = (int, int)
  lazy val Dir: Type = int
  lazy val Character: Type = (int, Loc, Dir, int, int)
  lazy val Ghost: Type = (int, Loc, Dir)
  lazy val Item: Type = int
  lazy val WorldMap: Type = ListType(ListType(Item))
  lazy val WorldState: Type = (WorldMap, Character, ListType(Ghost), int)

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
  lazy val worldApi = Seq(
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
  lazy val enumApi = Seq(
    fun('isWall)('obj % Item) { 'obj === item.wall },
    fun('isEmptyField)('obj % Item) { 'obj === item.empty },
    fun('isPill)('obj % Item) { 'obj === item.pill },
    fun('isPowerPill)('obj % Item) { 'obj === item.powerpill },
    fun('isFruit)('obj % Item) { 'obj === item.fruit },
    fun('isLambdaStart)('obj % Item) { 'obj === item.lambdaStart },
    fun('isGhostStart)('obj % Item) { 'obj === item.ghostStart }
  )

  lazy val item = new {
    val wall        = 0
    val empty       = 1
    val pill        = 2
    val powerpill   = 3
    val fruit       = 4
    val lambdaStart = 5
    val ghostStart  = 6
  }

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
  lazy val characterApi = Seq(
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
  lazy val directionApi = Seq(
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

    def filterNot(comp: UT /* T =>: bool */)
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
        'accPair.bind('acc, 'pos) {
          (if_(comp('el)) {
            ('el, 'pos) ::: 'acc
          } else_ {
            'acc
          }, 'pos + 1)
        }
      }).first
  }

  def HashMapType(KeyType: Type, ValueType: Type) =
    ClassType(
        KeyType =>: ValueType,
        KeyType =>: ValueType =>: UnitType,
        KeyType =>: bool,
        UnitType =>: tupleType(int, BinTreeType((KeyType, ValueType))),
        UnitType =>: bool,
        UnitType =>: ListType((KeyType, ValueType)))

  def HashMapClass(KeyType: Type, ValueType: Type)(name: Symbol): (Name, UT) =
    class_(name)('store % BinTreeType((KeyType, ValueType)), 'compare % (KeyType =>: KeyType =>: int)) (

      fun('get)('key % KeyType) {
        letrec {
          fun('getHelper)('store) {
            (if_('store.isEmptyTree) { debug(777) } else_ { noop }) ~:
            let('cmp, 'compare('store.nodeValue.first, 'key)) {
              if_('cmp === 0) {
                'store.nodeValue.second
              }. else_if ('cmp > 0) {
                'getHelper('store.rightTree)
              } else_ {
                'getHelper('store.leftTree)
              }
            }
          }
        }("getImpl", 'getHelper('store))
      },

      fun('put)('key % KeyType, 'value % ValueType) {
        letrec {
          fun('putHelper)('store) {
            if_('store.isEmptyTree) {
              leaf(('key, 'value))
            } else_ {
              let('cmp, 'compare('store.nodeValue.first, 'key)) {
                if_('cmp === 0) {
                  tree('store.leftTree, ('key, 'value), 'store.rightTree)
                }. else_if ('cmp > 0) {
                  tree('store.leftTree, 'store.nodeValue, 'putHelper('store.rightTree))
                } else_ {
                  tree('putHelper('store.leftTree), 'store.nodeValue, 'store.rightTree)
                }
              }
            }
          }
        }("putImpl", 'store <~ 'putHelper('store))
      },

      fun('isDefinedAt)('key % KeyType) {
        letrec {
          fun('definedAtHelper)('store) {
            not('store.isEmptyTree) and
            let('cmp, 'compare('store.nodeValue.first, 'key)) {
              if_('cmp === 0) {
                true
              }. else_if ('cmp > 0) {
                'definedAtHelper('store.rightTree)
              } else_ {
                'definedAtHelper('store.leftTree)
              }
            }
          }
        }("isDefinedAtImpl", 'definedAtHelper('store))
      },

      fun('print)('_ % UnitType) {
        (111111, 'store)
      },

      fun('isEmpty)('_ % UnitType) {
        'store.isEmptyTree
      },

      fun('toList)('_ % UnitType) {
        letrec (
          'listResult := empty ofType ListType((KeyType, ValueType)),

          fun('toListHelper)('store) {
            if_('store.isEmptyTree) {
              noop
            } else_ {
              'toListHelper('store.leftTree) ~:
              ('listResult <~ 'store.nodeValue ::: 'listResult) ~:
              'toListHelper('store.rightTree)
            }
          }
        )("toListImpl", 'toListHelper('store) ~: 'listResult)
      }
    )

  lazy val PointIntMap = HashMapType((int, int), int)
  lazy val PointPointMap = HashMapType((int, int), (int, int))

  // This contains only monomorphic functions, all polymorphic ones must be macros as above!
  lazy val collectionApi = Seq(
    fun('all)('list % ListType(BooleanType)) {
      foldRight('list, true, lam('b1, 'b2) { 'b1 and 'b2 })
    },
    fun('any)('list % ListType(BooleanType)) {
      foldRight('list, false, lam('b1, 'b2) { 'b1 or 'b2 })
    },

    HashMapClass((int, int), int)('PointIntMap),
    HashMapClass((int, int), (int, int))('PointPointMap)
  )
}

// requires Math and Points
trait Pathfinding extends SyntaxSugar with Points with Collection { self: LambdaManApi =>

  lazy val ParentMap = PointPointMap
  lazy val GMap      = PointIntMap
  lazy val Queue     = ListType(Point)

  lazy val initWalked = 0

  lazy val pathfinding = Seq(

    fun('manhattan)('from % Point, 'to % Point) {
      ('abs('from.x - 'to.x) + 'abs('from.y - 'to.y))
    },

    /**
     * Computes the path as list, starting with the next move
     * If no path could be found the list is empty
     */
    fun('computePath)('from % Point, 'to % Point, 'map % GameMap) {
      'extractPath('from, 'to, 'computeParents('from, 'map, 'distanceTo('to), 'isObstacle('map)))
    },

    fun('extractPath)('from % Point, 'to % Point, 'parents % ParentMap) {
      letrec (
        'extractPathGo := lam('curr, 'acc) {
          if_('pointEq('curr, 'from)) {
            'from ::: 'acc
          } else_{
            let('next, 'parents.call('PointPointMap, 'get)('curr)) {
              'extractPathGo('next, 'next ::: 'acc)
            }
          }
        }
      )("extractPathGo",
        if_('parents.call('PointPointMap, 'isEmpty)(unit)) {
          empty
        } else_ {
          'extractPathGo('to, empty).filterNot(lam('el){ 'pointEq('el, 'from) })
        })
    },

    'pathParamDg := 1,
    'pathParamDh := 1,

    fun('distanceTo)('target % Point) { noop ~: lam('p % Point) { 'pathParamDh * 'manhattan('p, 'target) } },
    fun('isObstacle)('map % GameMap) { noop ~: lam('p % Point) {  ('map atPos 'p) =!= 0  } },
    fun('inRange)('map % GameMap, 'p % Point) {
      letS(
        'limit := (('map.head.size - 1, 'map.size - 1))
      ) {
        'p.x >= 0 and 'p.y >= 0 and 'p.x <= 'limit.x and 'p.y <= 'limit.y
      }
    },

    /**
     * Main function for A*
     *
     * @param start the point to start the path search from
     * @param map a two dimensional list representing the game map
     * @param distance the approximated distance to the desired goal (point => int)
     * @param shouldConsider a predicate that tests for obstacles
     */
    fun('computeParents)('start % Point, 'map % GameMap, 'distance % (Point =>: int), 'shouldConsider % (Point =>: bool)) {
      letS(

        'parents := 'new_PointPointMap(emptyTree, 'pointCompare) ofType ParentMap,
        'gMap    := 'new_PointIntMap(emptyTree, 'pointCompare) ofType GMap,

        // distance measures
        // TODO use getOrElseUpdate here
        'G := lam('a % Point) {
          if_('gMap.call('PointIntMap, 'isDefinedAt)('a)) {
            'gMap.call('PointIntMap, 'get)('a)
          } else_ {
            'pathParamDg * 'manhattan('a, 'start)
          }
        },
        'F := lam('a % Point) { 'G('a) + 'distance('a) }
      ) {
        letrec {
          //Invariant: 'curCell is inside 'closedList
          fun('loop)('curCell % Point, 'openList % Queue, 'closedList % Queue, 'walked % int) {
            letS(
              'Ncell := 'curCell |+| (( 0, -1)),
              'Scell := 'curCell |+| (( 0,  1)),
              'Ecell := 'curCell |+| (( 1,  0)),
              'Wcell := 'curCell |+| ((-1,  0)),
              'neighbours := list('Ncell, 'Scell, 'Ecell, 'Wcell),

              'alreadyHandled := lam('p % Point) { 'closedList contains ('p, 'pointEq) },

               /**
                * Updates the parents, openList and gMap for the given cell
                *
                * Returns the updated localState
                */
              'checkCell := lam('p % Point, 'openList % Queue) {

                if_('inRange('map, 'p) and not('alreadyHandled('p)) and 'shouldConsider('p)) {
                  if_(not('openList contains ('p, 'pointEq))) {
                    'parents.call('PointPointMap, 'put)('p, 'curCell) ~:
                    ('p ::: 'openList)
                  } else_ {
                    letS(
                      'gvalCur := 'G('curCell) + 'pathParamDg,
                      'gvalP := 'G('p)
                    ) {
                      // Showing these values shows
                      // we enter here seldom, if ever,
                      // since we seldom compare different paths to the
                      // same point.
                      //debug('gvalCur) ~: debug('gvalP) ~:
                      (if_('gvalCur < 'gvalP) {
                        // update parents and gmap
                        'gMap.call('PointIntMap, 'put)('p, 'gvalCur) ~:
                        'parents.call('PointPointMap, 'put)('p, 'curCell) ~:
                        'openList
                      } else_ {
                        'openList
                      })
                    }
                  }
                } else_ {
                  'openList // did not change anything
                }

              },

              /**
               * In a list of points searches the minimum comparing the F values
               * The provided list must not be empty!
               *
               * Returns the found minimum point
               */
              'findCellWithSmallestF := lam('list % Queue) {
                  foldRight('list, 'list.head, lam('el, 'curMax) {
                    if_('F('el) < 'F('curMax)) { 'el } else_ { 'curMax }
                 })
               },

               'openList := foldRight('neighbours, 'openList, 'checkCell)
            ) {
              if_('openList.isEmpty) {
                'parents // We are done, all cells have been visited
              } else_{
                letS(
                  'nextCell       := 'findCellWithSmallestF('openList),
                  'nextOpenList   := 'openList.filterNot(lam('el){ 'pointEq('el, 'nextCell) }),
                  'nextClosedList := 'nextCell ::: 'closedList
                ) {
                  if_('distance('nextCell) === 0) {
                    // Show map of weights.
                    // debug('gMap) ~:
                    'parents
                  } else_ {
                    // Store in map how much it took to get to 'nextCell.
                    // This makes the 'G cost function more precise.
                    'gMap.call('PointIntMap, 'put)('nextCell, 'walked) ~:
                    'loop('nextCell, 'nextOpenList, 'nextClosedList, 'walked + 1)
                  }
                }
              }
            }
          }
        }("pathfindingBody", 'loop('start, empty, 'start ::: empty, initWalked))
      }
    }
  )
}

trait Math extends SyntaxSugar {

  lazy val math = Seq(
    fun('mod)('x % int, 'y % int) { 'x - ('x / 'y * 'y) },
    fun('abs)('x % int) { if_('x < 0) { 'x * (-1) } else_ { 'x } }
  )

}


trait Points extends SyntaxSugar with Collection { outer: LambdaManApi =>

  lazy val Point: Type = (int, int)
  lazy val GameMap: Type = ListType(ListType(int))

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

  lazy val points = Seq(
    fun('pointEq)('p1 % Point, 'p2 % Point) { ('p1.x === 'p2.x) and ('p1.y === 'p2.y) },

    // We know that the map size is at most 256 * 256
    fun('pointHash)('p % Point) { 'p.x * 256 + 'p.y },
    fun('decode)('hash % int) { ('hash / 256, 'mod('hash, 256)) },

    // 0 if p1 == p2
    fun('pointCompare)('p1 % Point, 'p2 % Point) { 'pointHash('p2) - 'pointHash('p1) },

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