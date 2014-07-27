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
trait LambdaManApi extends SyntaxSugar with Math {

  lazy val all = collectionApi ++ worldApi ++ enumApi ++ characterApi ++ directionApi ++ math

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

  def elemAt(list: UT, i: UT) =
    letrec {
        fun('go)('l, 'i) {
          if_('i === 0) {
            'l.head
          } else_ {
            'go('l.tail, 'i - 1)
          }
        }
      }("elemAtBody", 'go(list, i))

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

  //This contains only monomorphic functions, all polymorphic ones must be macros as above!
  val collectionApi = Seq(
    fun('all)('list % ListType(BooleanType)) {
      foldRight('list, true, lam('b1, 'b2) { 'b1 and 'b2 })
    },
    fun('any)('list % ListType(BooleanType)) {
      foldRight('list, false, lam('b1, 'b2) { 'b1 or 'b2 })
    }
  )

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

trait Math extends SyntaxSugar {

  val math = Seq(
    fun('mod)('x % int, 'y % int) { 'x - ('x / 'y * 'y) }
  )

}
