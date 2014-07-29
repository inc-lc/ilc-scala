package ilc.language
package gcc

import Predef.{any2stringadd => _, _}
import scala.language.{ implicitConversions, reflectiveCalls }

class ProgramBase extends GCC {


  lazy val Strategy: Type = Point =>: StrategyState =>: WorldMap =>: Path
    // Recursive types don't work
  lazy val StrategyState: Type = (Dir, int, Point, Path, ListType(Point))
  lazy val Path: Type = ListType(Point)
  lazy val AIState: Type = tupleType(Dir, int, Point, Path, Strategy, ListType(Point))
  lazy val initialState = tuple(
      move.left,    // current direction
      0,            // tick
      (0, 0),       // fruit position
      empty,        // planned Path
      'collectCoins, // active strategy,
      empty          // unvisted locations
  ) ofType AIState
  lazy val stateSize = 6


  lazy val aiStateApi = Seq(
    fun('getMovement)('_state % AIState) { '_state.at(0, stateSize) },
    fun('getTick)('_state % AIState) { '_state.at(1, stateSize) },
    fun('getFruitPosition)('_state % AIState) { '_state.at(2, stateSize) }
  )

  lazy val mapApi = Seq(
  )

  lazy val scores = Seq(
    'scores_pill      := 10,
    'scores_powerpill := 50,

    'scores_fruit     := lam('map) {
      letS(
        'height := 'map.size,
        'width  := 'map.head.size,
        'lvl    := (50 + 'width * 'height) / 100
      )(switch('lvl) (
         1 ==> 100,
         2 ==> 300,
         3 ==> 500,
         4 ==> 500,
         5 ==> 700,
         6 ==> 700,
         7 ==> 1000,
         8 ==> 1000,
         9 ==> 2000,
        10 ==> 2000,
        11 ==> 3000,
        12 ==> 3000
      ) withDefault (5000))
    },

    'scores_ghost := lam('n) {
      switch('n) (
        1 ==> 200,
        2 ==> 400,
        3 ==> 800
      ) withDefault (1600)
    }
  )

  // A strategy is a function that returns a path
  lazy val strategies = Seq(

      'viewRadius := 5,

      'collectCoins := lam('currentPos % Point, 'state % StrategyState, 'map % WorldMap) {
        'state.bind('currDir % Dir, 'tick % int, 'fruitPos % Point, 'plannedPath % Path, 'unvisted % ListType(Point)) {

          letS(
            // see what can be reached
            'nearBy := 'distanceMapNearPOI('map, 'currentPos, 'viewRadius),

            // maybe this costs too much performance?
            'allPaths := map('nearBy, lam('t) {
              'extractPath('currentPos, 'decode('t.first), 'nearBy)
            }),

            'pathValue := lam('path) { foldRight('path, 0, lam('pos, 'sum) { 'sum + 'valueOfPos('pos, 'map) }) },

            // find path with maximum value
            'bestPath := foldRight('allPaths, (empty, -1), lam('path, 'pathAndOldVal) { 'pathAndOldVal.bind('oldPath, 'oldVal) {
              let('value, 'pathValue('path)) {
                if_('value > 'oldVal) {
                  ('path, 'value)
                } else_ {
                  'pathAndOldVal
                }
              }
            }}).first

          )('bestPath)
        }
      } ofType Strategy,

      // Strategy: Closest unvisted field
      'closestUnvisted := lam('currentPos % Point, 'state % StrategyState, 'map % WorldMap) {
        'state.bind('currDir % Dir, 'tick % int, 'fruitPos % Point, 'plannedPath % Path, 'unvisted % ListType(Point)) {
          let('target,
            foldRight('unvisted, (999, (-1, -1)), lam('p, 'acc) { 'acc.bind('bestDist, 'bestPoint) {
              let('thisDist, 'manhattan('currentPos, 'p)) {
                if_('thisDist < 'bestDist) {
                  ('thisDist, 'p)
                } else_ {
                  'acc
                }
              }
            }}).second) {
              'computePath('currentPos, 'target, 'map)
            }
        }
      } ofType Strategy
      // pseudo-random move, depends on the tick
//      fun('randomMove)('currentPos % Point, 'state % StrategyState, 'map % WorldMap) {
//        ('mod('getTick('state), 4) ofType Dir) ::: empty
//      }

  )

  // A tactic is a function that evaluates whether we need a new strategy
  lazy val tactics = Seq(

  )

  lazy val helpers = Seq(
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

    fun('valueOfPos)('pos % Point, 'map % WorldMap) {
      switch('map atPos 'pos) (
        item.pill      ==> 'scores_pill,
        item.powerpill ==> 'scores_powerpill
        //item.fruit     ==> 'scores_fruit('map) - also have to take fruit presents into account
      ) withDefault 0
    },

    fun('obstacleAt)('world % WorldState, 'x % IntType, 'y % IntType) {
      letS (
        'item           := 'world_itemAt('world, 'x, 'y),
        'ghostStatuses  := 'world_ghostsStatus('world),
        'ghostLocations := map('ghostStatuses, 'location),
        'ghostsInPos    := 'any(map('ghostLocations, lam('pos % Loc) {
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

//    fun('chooseFreeDir)('world % WorldState, '_state % AIState) {
//      letrec(
//        fun('go)('mov % Dir) {
//          if_(not('obstacleInDir('world, 'mov))) {
//            'mov
//          } else_ {
//            let ('nextMov, 'randomMove('world, '_state)) {
//              if_ ('mov =!= 'nextMov) {
//                'go('nextMov)
//              } else_ {
//                'go('nextDir('mov))
//              }
//            }
//          }
//        })("chooseFreeDir",
//          'go('getMovement('_state)))
//    },

    // idea use the same mechanism to figure out whether a ghost can reach us
    fun('distanceMapNearPOI)('map % GameMap, 'poi % Point, 'maxAttentionRadius % int) {

      letS(
         // we use constant 1 to treat all cells the same
        'distance := lam('p % Point) { 1 },

        // this function determines whether a point should be considered
        'consider := lam('p % Point) {
          'withinRadius('poi, 'maxAttentionRadius, 'p) and 'inRange('map, 'p) and (('map atPos 'p) =!= 0)
        }
      )('computeParents('poi, 'map, 'distance, 'consider))
    },

    fun('withinRadius)('c % Point, 'radius %int, 'p % Point) {
      let('delta, 'c |-| 'p) {
        ('delta.x * 'delta.x + 'delta.y * 'delta.y) <= ('radius * 'radius)
      }
    },

    /**
     * Collects all cells in the which could be used as targets
     */
    fun('collectUnvistedCells)('map % GameMap) {
      foldRight('map, ('map.size - 1, empty), lam('row, 'acc1) { 'acc1.bind('i, 'cells) {
        ('i - 1, foldRight('row, ('row.size - 1, 'cells), lam('cell, 'acc2) { 'acc2.bind('j, 'cells) {
          if_('cell === item.wall) { ('j - 1, 'cells) } else_ { ('j - 1, ('j, 'i) ::: 'cells) }
        }}).second)
      }}).second
    },

    // Typechecker stack overflows
    fun('pickStrategy)('currentPos % Point, 'strategyState % StrategyState, 'map % GameMap) {
      letrec(
        'pickFrom := lam('strategies % ListType(Strategy)) {
          letS(
            // TODO check whether we exhausted our strategies...
            'strategy := 'strategies.head,
            'path     := 'strategy('currentPos, 'strategyState, 'map)
          ) (if_('path.isEmpty) { 'pickFrom('strategies.tail) } else_{ ('strategy, 'path) })
        } ofType( ListType(Strategy) =>: tupleType(Strategy, Path))
      )("strategyPicking", 'pickFrom(list('collectCoins, 'closestUnvisted))) ofType ((Strategy, Path))
    }

  )

  /*
   * Look for the next thing among:
   * - ghosts (in fright mode)
   * - fruit
   * - power pill [ if ghosts are nearby ? ]
   * - pills
   */
  def targetPosition(xSize: UT, ySize: UT): UT = tuple(1, ySize - 2)
  def otherTargetPositions(xSize: UT, ySize: UT) = list(tuple(1, 1), tuple(xSize - 2, 1), tuple(xSize - 2, ySize - 2))

  lazy val main = letrec((all ++ helpers ++ strategies ++ aiStateApi ++ scores): _*)("main",

    // First component: The initalized world state
    (initialState.bind('dir, 'tick, 'fruitPos, '_, '_, '_) {
      letS(
          'map    := 'world_map('initWorld ofType WorldState),
          // 'poiMap := 'distanceMapNearPOI('map, (1, 3), 15),
          'ySize  := size('map),
          'xSize  := size('map.head),

          'unvisited := 'collectUnvistedCells('map)
        ){ tuple('dir, 'tick, 'fruitPos, empty, 'collectCoins, 'unvisited)  }
    },

    // second component: The step function
    lam('state % AIState, 'world % WorldState) {

      'state.bind('currDir % Dir, 'tick % int, 'fruitPos % Point, 'path % Path, 'strategy % Strategy, 'unvisted % ListType(Point)) {

        letS(
            'currentPos    := 'location('world_lambdaStatus('world)),
            'map           := 'world_map('world),
            'nextUnvisted  := 'unvisted.filterNot(lam('el){ 'pointEq('el, 'currentPos) }),
            'strategyState := tuple('currDir, 'tick, 'fruitPos, 'path, 'unvisted),

            // potentially new path and strategy
            'pathStrategy  := (if_('path.isEmpty) { 'pickStrategy('currentPos, 'strategyState, 'map) } else_ { ('path, 'strategy) }),
            'path          := 'pathStrategy.first,
            'strategy      := 'pathStrategy.second,

            'nextPos       := 'path.head,

            // TODO insert tactics here (like: Avoid ghosts)
            'nextDir       := 'currentPos moveTo 'nextPos

            // TODO lambda man can die, then we have to reevaluate the strategy...

            // TODO if strategy does return empty path we have to switch to some other strategy like: "Try to go up/left/right/down"
        ) {
           (tuple('nextDir, 'mod('tick + 1, 1337), 'fruitPos, 'path.tail, 'strategy, 'nextUnvisted), 'nextDir)
        }
      }
    }))

  lazy val cp @ CompiledProgram(prog, labels) = toProg(typecheck(main))

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
