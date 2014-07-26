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
    fun('world_map)('world) { 'world.at(0, 4) },
    fun('world_lambdaStatus)('world) { 'world at(1, 4) },
    fun('world_itemAt)('world, 'x, 'y) { elemAt(elemAt('world at(0, 4), 'y), 'x) },

    /**
     * Returns a list of ghosts
     *
     * We pad it to a 5-tuple in order to be compatible with lambda man
     */
    fun('world_ghostsStatus)('world) {
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
    fun('world_fruitStatus)('world) { 'world at(3, 4) }
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
    fun('isWall)('obj) { 'obj === 0 },
    fun('isEmptyField)('obj) { 'obj === 1 },
    fun('isPill)('obj) { 'obj === 2 },
    fun('isPowerPill)('obj) { 'obj === 3 },
    fun('isFruit)('obj) { 'obj === 4 },
    fun('isLambdaStart)('obj) { 'obj === 5 },
    fun('isGhostStart)('obj) { 'obj === 6 }
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
    fun('vitality)('char) { 'char at(0, 5) },
    fun('location)('char) { 'char at(1, 5) },
    fun('direction)('char) { 'char at(2, 5) },

    /**
     * Only works for lambdas!
     */
    fun('lives)('char) { 'char at(3, 5) },
    fun('score)('char) { 'char at(4, 5) },

    // for ghosts:
    fun('isAfraid)('ghost) { 'vitality('ghost) === 1 },
    fun('isInvisible)('ghost) { 'vitality('ghost) === 2 },

    // for lambda man:
    fun('powerLeft)('lambdaMan) { 'lambdaMan at(0, 5) },
    fun('isInPowerMode)('lambdaMan) { 'powerLeft('lambdaMan) =!= 0 }
  )

  /**
   *  The Ghosts' and Lambda-Man's direction is an enumeration:
   * - 0: up;
   * - 1: right;
   * - 2: down;
   * - 3: left.
   */
  val directionApi = Seq(
    fun('isUp)('obj) { 'obj === 0 },
    fun('isRight)('obj) { 'obj === 1 },
    fun('isDown)('obj) { 'obj === 2 },
    fun('isLeft)('obj) { 'obj === 3 }
  )

}