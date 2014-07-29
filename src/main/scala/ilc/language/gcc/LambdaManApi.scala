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

  // The type of a very primitive hash map with O(n) lookup but O(1) update
  // still should yield performance improvements on lists of lists which are mostly empty...
  def HashMap(t: Type) = ListType((int, t))

  implicit class HashMapOps[T <: UT](hmap: T /* Map[K, V] */) {

    // we simply never GC old keys... Should work for short lived maps
    def put(key: UT /* K */, value: UT /* V */, hashfun: UT /* K => int */) =
      (hashfun(key), value) ::: hmap

    def get(key: UT, hashfun: UT) =
      letrec {
        fun('hmapGet)('map, 'hashedKey % int) {
          //XXX handle empty case and raise appropriate error
          'map.head.bind('key, 'value) {
            if_('key === 'hashedKey) { 'value } else_{ 'hmapGet('map.tail, 'hashedKey) }
          }
        }
      }("hmapGet", 'hmapGet(hmap, hashfun(key)))

    def isDefinedAt(key: UT, hashfun: UT) =
      letrec {
        fun('hmapisDefinedAt)('map, 'hashedKey % int) {
          if_('map.isEmpty) {
            false
          } else_ {
            'map.head.bind('key, 'value) {
              ('key === 'hashedKey) or 'hmapisDefinedAt('map.tail, 'hashedKey)
            }
          }
        }
      }("hmapisDefinedAt", 'hmapisDefinedAt(hmap, hashfun(key)))
  }
  def hashmap(t: UT): HashMapOps[UT] = t


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

    def filterNot[T](comp: UT /* T =>: bool */)
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
}

// requires Math and Points
trait Pathfinding extends SyntaxSugar with Points with Collection { self: LambdaManApi =>

  lazy val ParentMap = HashMap(Point) // Map[Point, Point]
  lazy val GMap      = HashMap(int) // Map[Point, int]
  lazy val Queue     = ListType(Point)

  // used in loop body
  lazy val LocalState = (ParentMap, Queue, GMap)

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
      letrec (
        'parents := 'computeParents('from, 'map, 'distanceTo('to), 'isObstacle('map)),
        'computePathGo := lam('curr, 'acc) {
          if_('pointEq('curr, 'from)) {
            'from ::: 'acc
          } else_{
            let('next, hashmap('parents).get('curr, 'pointHash)) {
              'computePathGo('next, 'next ::: 'acc)
            }
          }
        }
      )("computePathBody", if_('parents.isEmpty) { empty } else_ { 'computePathGo('to, empty).filterNot(lam('el){ 'pointEq('el, 'from) }) })

    },

    'pathParamDg := 1,
    'pathParamDh := 1,

    fun('distanceTo)('target % Point) { noop ~: lam('p % Point) { 'pathParamDh * 'manhattan('p, 'target) } },
    fun('isObstacle)('map % GameMap) { noop ~: lam('p % Point) {  ('map atPos 'p) =!= 0  } },

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
        // distance measures
        'G := lam('a % Point, 'gMap % GMap) {
          if_(hashmap('gMap) isDefinedAt ('a, 'pointHash)) {
            hashmap('gMap).get('a, 'pointHash)
          } else_ {
            'pathParamDg * 'manhattan('a, 'start)
          }
        },
        'F := lam('a % Point, 'gMap % GMap) { 'G('a, 'gMap) + 'distance('a) },

        // the last cells of the map
        'limit := (('map.head.size - 1, 'map.size - 1)),
        'inRange := lam('p % Point) { 'p.x >= 0 and 'p.y >= 0 and 'p.x <= 'limit.x and 'p.y <= 'limit.y }
      ) {
        letrec {
          //Invariant: 'curCell is inside 'closedList
          fun('loop)('curCell % Point, 'openList % Queue, 'closedList % Queue, 'parents % ParentMap, 'gMap % GMap, 'walked % int) {
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
              'checkCell := lam('p % Point, 'localState % LocalState) {

                'localState.bind('parents, 'openList, 'gMap) {
                  if_('inRange('p) and not('alreadyHandled('p)) and 'shouldConsider('p)) {
                    if_(not('openList contains ('p, 'pointEq))) {
                      tuple(hashmap('parents).put('p, 'curCell, 'pointHash), 'p ::: 'openList, 'gMap)
                    } else_ {
                      letS(
                          'gvalCur := 'G('curCell, 'gMap) + 'pathParamDg,
                          'gvalP := 'G('p, 'gMap)
                          ) {
                        // Showing these values shows
                        // we enter here seldom, if ever,
                        // since we seldom compare different paths to the
                        // same point.
                        //debug('gvalCur) ~: debug('gvalP) ~:
                        (if_('gvalCur < 'gvalP) {
                          // update parents and gmap
                          tuple(
                              hashmap('parents).put('p, 'curCell, 'pointHash),
                              'openList,
                              hashmap('gMap).put('p, 'gvalCur, 'pointHash))
                        } else_ {
                          tuple('parents, 'openList, 'gMap)
                        })
                      }
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

               'nextState := foldRight('neighbours, tuple('parents, 'openList, 'gMap), 'checkCell)
            ) {
              'nextState.bind('parents, 'openList, 'gMap) {
                if_('openList.isEmpty) {
                  empty //return error! nothing found...
                } else_{
                  letS(
                    'nextCell       := 'findCellWithSmallestF('openList, 'gMap),
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
                      letS (
                        'updGMap := hashmap('gMap).put('nextCell, 'walked, 'pointHash)
                      ) {
                        'loop('nextCell, 'nextOpenList, 'nextClosedList, 'parents, 'updGMap, 'walked + 1)
                      }
                    }
                  }
                }
              }
            }
          }
        }("pathfindingBody", 'loop('start, empty, 'start ::: empty, empty, empty, initWalked))
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