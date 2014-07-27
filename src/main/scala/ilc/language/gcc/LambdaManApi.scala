package ilc
package language
package gcc

import feature._
import scala.language.implicitConversions

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
trait LambdaManApi extends SyntaxSugar {

  lazy val all = collections ++ worldApi ++ enumApi ++ characterApi ++ directionApi

  val Loc: Type = ProductType(IntType, IntType)
  val Dir: Type = IntType
  val Character: Type = tupleType(IntType, Loc, Dir, IntType, IntType)
  val Ghost: Type = tupleType(IntType, Loc, Dir)
  val Item: Type = IntType
  val WorldMap: Type = ListType(ListType(Item))
  val WorldState: Type = tupleType(WorldMap, Character, ListType(Ghost), IntType)

  object move {
    val up    = 0
    val right = 1
    val down  = 2
    val left  = 3
  }

  def elemAt(list: UntypedTerm, i: UntypedTerm) =
    letrec {
        fun('go)('l, 'i) {
          if_('i === 0) {
            'l.head
          } else_ {
            'go('l.tail, 'i - 1)
          }
        }
      }("elemAtBody", 'go(list, i))

  // TODO reimplement those as macros such as elemAt
  val collections = Seq(
    fun('foldRight)('list, 'z, 'fun) {
      letrec {
        fun('go)('l) {
          if_('l.isEmpty) {
            'z
          } else_ {
            'fun('l.head, 'go('l.tail))
          }
        }
      }("foldRightBody", 'go('list))
    },

    fun('map)('list, 'fun) {
      'foldRight('list, empty, lam('head, 'tail) {
        'fun('head) ::: 'tail
      })
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
    funT('world_map)('world % WorldState) { 'world.at(0, 4) },
    funT('world_lambdaStatus)('world % WorldState) { 'world at(1, 4) },
    funT('world_itemAt)('world % WorldState, 'x % IntType, 'y % IntType) { elemAt(elemAt('world at(0, 4), 'y), 'x) },

    /**
     * Returns a list of ghosts
     *
     * We pad it to a 5-tuple in order to be compatible with lambda man
     */
    funT('world_ghostsStatus)('world % WorldState) {
      'map('world at(2, 4), lam('el) {
        tuple('el at(0, 3), 'el at(1, 3), 'el at(2, 3), 0, 0)
      })
    },

    /**
     * The status of the fruit is a number which is a countdown to the expiry of
     * the current fruit, if any.
     * - 0: no fruit present;
     * - n > 0: fruit present: the number of game ticks remaining while the
     *          fruit will will be present.
     */
    funT('world_fruitStatus)('world % WorldState) { 'world at(3, 4) }
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
    funT('isWall)('obj % Item) { 'obj === 0 },
    funT('isEmptyField)('obj % Item) { 'obj === 1 },
    funT('isPill)('obj % Item) { 'obj === 2 },
    funT('isPowerPill)('obj % Item) { 'obj === 3 },
    funT('isFruit)('obj % Item) { 'obj === 4 },
    funT('isLambdaStart)('obj % Item) { 'obj === 5 },
    funT('isGhostStart)('obj % Item) { 'obj === 6 }
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
    funT('vitality)('char % Character) { 'char at(0, 5) },
    funT('location)('char % Character) { 'char at(1, 5) },
    funT('direction)('char % Character) { 'char at(2, 5) },

    /**
     * Only works for lambdas!
     */
    funT('lives)('char % Character) { 'char at(3, 5) },
    funT('score)('char % Character) { 'char at(4, 5) },

    // for ghosts:
    funT('isAfraid)('ghost % Character) { 'vitality('ghost) === 1 },
    funT('isInvisible)('ghost % Character) { 'vitality('ghost) === 2 },

    // for lambda man:
    funT('powerLeft)('lambdaMan % Character) { 'lambdaMan at(0, 5) },
    funT('isInPowerMode)('lambdaMan % Character) { 'powerLeft('lambdaMan) =!= 0 }
  )

  /**
   *  The Ghosts' and Lambda-Man's direction is an enumeration:
   * - 0: up;
   * - 1: right;
   * - 2: down;
   * - 3: left.
   */
  val directionApi = Seq(
    funT('isUp)('obj % Dir) { 'obj === move.up },
    funT('isRight)('obj % Dir) { 'obj === move.right },
    funT('isDown)('obj % Dir) { 'obj === move.down },
    funT('isLeft)('obj % Dir) { 'obj === move.left }
  )

}