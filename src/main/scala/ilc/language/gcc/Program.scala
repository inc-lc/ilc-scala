package ilc.language
package gcc

import Predef.{any2stringadd => _, _}

class ProgramBase extends GCC {

  val program = Seq(
    //fun('go)('world, 'ghosts) { }
  )

  // TODO add debug statement

  val AIState: Type = (Dir, int, int, int)
  val initialState = tuple(move.left, 0, 0, 0) ofType AIState
  val stateSize = 4


  val helpers = Seq(
    fun('test)('a % bool, 'b % bool) {
      ('a or 'b) and 'a
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
         if_('dir === move.left) {
          ('loc.first - 1, 'loc.second)
         } .else_if ('dir === move.right) {
          ('loc.first + 1, 'loc.second)
         } .else_if ('dir === move.up) {
          ('loc.first, 'loc.second - 1)
         } else_ {
          ('loc.first, 'loc.second + 1)
         } ofType Loc) {
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
            'go('nextDir('mov))
          }
        })("chooseFreeDir",
          'go('myMovement('_state)))
    },

    fun('myMovement)('_state % AIState) { '_state.at(0, stateSize) }

  )


  val main = letrec((all ++ helpers ++ program): _*)("main",
      (initialState, lam('_state % AIState, 'world % WorldState) {
        let('nextDir, 'chooseFreeDir('world, '_state)) {
           (tuple('nextDir, 0, 0, 0), 'nextDir)
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

  //println("[" + (resolveSymbolic(prog, labels) map (_ show(true)) mkString ",\n") + "]")
  def showProg() = println(cp.toRaw)

}

//To load in REPL
object Program extends ProgramBase {
  showProg()
}

//To actually run.
object Main extends App {
  (new ProgramBase()).showProg
}
