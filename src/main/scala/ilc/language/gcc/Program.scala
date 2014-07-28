package ilc.language
package gcc

import Predef.{any2stringadd => _, _}

class ProgramBase extends GCC {

  val program = Seq(
    //fun('go)('world, 'ghosts) { }
  )

  // TODO add debug statement

  val AIState: Type = (Dir, int, Point, ListType(Point))
  val initialState = tuple(
      move.left, // current direction
      0, // tick
      (0, 0), //fruit position
      empty) ofType AIState
  val stateSize = 4

  val mapApi = Seq(
  )

  val helpers = Seq(
    fun('test)('a % bool, 'b % bool) {
      ('a or 'b) and 'a
    },

    fun('findFruitLocation)('map % WorldMap) {
      val foundColumns = map('map, lam('row) { 'row search { lam('cell)('isFruit('cell)) }})
      val foundFruit = foundColumns search { lam('row) { not('row.isEmpty) } }
      let('head, foundFruit.head) {
        ('head.second, 'head.first.head.second)
      }
      // X and Y
    },

    fun('obstacleAt)('world % WorldState, 'x % IntType, 'y % IntType) {
      letS (
        'item := 'world_itemAt('world, 'x, 'y),
        'ghostStatuses := 'world_ghostsStatus('world),
        'ghostLocations := map('ghostStatuses, 'location),
        'ghostsInPos := 'any(map('ghostLocations, lam('pos % Loc) {
          'pos.first === 'x and 'pos.second === 'y
        }))
      ) {
        'isWall('item) or 'ghostsInPos
      }
    },

    fun('obstacleInDir)('world % WorldState, 'dir % Dir) {

      let('loc, 'location('world_lambdaStatus('world) ofType Character) ofType Loc) {
      let('nextPos,
         switch ('dir) (
           move.left  ==> (('loc.first - 1, 'loc.second)),
           move.right ==> (('loc.first + 1, 'loc.second)),
           move.up    ==> (('loc.first, 'loc.second - 1))
         ) withDefault(('loc.first, 'loc.second + 1)) ofType Loc ) {
          'obstacleAt('world, 'nextPos.first, 'nextPos.second) ofType bool
        }
      }
    },

    fun('chooseFreeDir)('world % WorldState, '_state % AIState) {
      letrec(
        fun('go)('mov % Dir) {
          if_(not('obstacleInDir('world, 'mov))) {
            'mov
          } else_ {
            let ('nextMov, 'randomMove('world, '_state)) {
              if_ ('mov =!= 'nextMov) {
                'go('nextMov)
              } else_ {
                'go('nextDir('mov))
              }
            }
          }
        })("chooseFreeDir",
          'go('getMovement('_state)))
    },

    fun('getMovement)('_state % AIState) { '_state.at(0, stateSize) },
    fun('getTick)('_state % AIState) { '_state.at(1, stateSize) },

    // pseudo-random move, depends on the tick
    fun('randomMove)('world % WorldState, '_state % AIState) {
      ('mod('getTick('_state), 4) ofType Dir)
    }

  )

  /*
   * Look for the next thing among:
   * - ghosts (in fright mode)
   * - fruit
   * - power pill [ if ghosts are nearby ? ]
   * - pills
   */
  val targetPosition: UT = tuple(1, 1)
  def otherTargetPositions(xSize: UT, ySize: UT) = list(tuple(xSize - 2, 1), tuple(xSize - 2, ySize - 2), tuple(1, ySize - 2))

  val main = letrec((all ++ helpers ++ program): _*)("main",
    (initialState.bind('dir, 'tick, 'fruitPos, 'dum) {
      letS(
          'map := 'world_map('initWorld ofType WorldState),
          'ySize := size('map),
          'xSize := size('map.head)
          ){
        tuple('dir, 'tick,
          targetPosition,
          ///*'fruitPos 'findFruitLocation(*/'world_map('initWorld ofType WorldState),
          otherTargetPositions('xSize, 'ySize))
      }
    },
          lam('_state % AIState, 'world % WorldState) {

        '_state.bind('currDir % Dir, 'tick % int, 'dest, 'otherDests) {

          letS(
              'currentPos := 'location('world_lambdaStatus('world)),
              'newDestsPair := if_ ('pointEq('currentPos, 'dest)) { ('otherDests.head, 'otherDests.tail) } else_ { ('dest, 'otherDests) },
              'newDest := 'newDestsPair.first,
              'newOtherDests := 'newDestsPair.second,
              // TODO this is really expensive, should not be computed every tick... only if strategy changes
              'path := 'computePath('currentPos, 'newDest, 'world_map('world)),
              'nextPos := 'path.head, // TODO check for empty...
              'nextDir := 'currentPos moveTo 'nextPos/*'chooseFreeDir('world, '_state)*/
          ) {
             (tuple('nextDir, 'mod('tick + 1, 1337), 'newDest, 'newOtherDests), 'nextDir)
          }
        }
      }))

//    val main = letrec((all ++ helpers ++ program): _*)("main",
//      (initialState, lam('_state, 'world) {
//        let('mov, True) {
//        let('nextDir,
//            //elemAt(elemAt(list(list(1,2,3),list(4,5,6),list(0,1,4)), 1), 1)
//            //'obstacleInDir('world, move.left)
//            'location('world_lambdaStatus('world))
//        ) {
//           (tuple(2, 0, 0, 0),
//            if_('mov) {
//              2
//            } else_ {
//              4
//            })
//        }}
//      }))

  val cp @ CompiledProgram(prog, labels) = toProg(typecheck(main))

  def results = cp.toRaw

  //println("[" + (resolveSymbolic(prog, labels) map (_ show(true)) mkString ",\n") + "]")
  def showProg() = println(results)

}

//To load in REPL
object Program extends ProgramBase {
  showProg()
}

//To actually run.
object Main extends App {
  (new ProgramBase()).showProg
}
