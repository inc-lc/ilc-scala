package ilc.language
package gcc

import Predef.{any2stringadd => _, _}

object Program extends GCC {

  val program = Seq(
    //fun('go)('world, 'ghosts) { }
  )

  // TODO implement "or"
  // TODO implicitly convert booleans
  // TODO add debug statement

  val AIState: Type = tupleType(Dir, IntType, IntType, IntType)
  val initialState = tuple(move.left, 0, 0, 0)
  val stateSize = 4


  val helpers = Seq(
    funT('obstacleAt)('world % WorldState, 'x % IntType, 'y % IntType) {
      let('item, 'world_itemAt('world, 'x, 'y)) {
        if_('isWall('item)) {
          True
        // TODO add ghosts here...
        } else_ {
          False
        }
      }
    },

    funT('obstacleInDir)('world % WorldState, 'dir % Dir) {
      let('loc, 'location('world_lambdaStatus('world) ofType Loc)) {
      let('nextPos,
         if_('dir === move.left) {
          ('loc.first - 1, 'loc.second)
         } else_ (if_('dir === move.right) {
          ('loc.first + 1, 'loc.second)
         } else_ (if_('dir === move.up) {
          ('loc.first, 'loc.second - 1)
         } else_ {
          ('loc.first, 'loc.second + 1)
         })) ofType Loc) {
          'obstacleAt('world, 'nextPos.first, 'nextPos.second) ofType BooleanType
        }
      }
    },

    funT('chooseFreeDir)('world % WorldState, '_state % AIState) {
      if_(not('obstacleInDir('world, '_state, move.right))) {
        move.right
      } else_ (if_(not('obstacleInDir('world, '_state, move.left))) {
        move.left
      } else_ (if_(not('obstacleInDir('world, '_state, move.up))) {
        move.up
      } else_ {
        move.down
      }))
    },

    funT('myMovement)('_state % AIState) { '_state.at(0, stateSize) }

  )


  val main = letrec((all ++ helpers ++ program): _*)("main",
      (initialState, lam('_state, 'world) {
        let('mov, 'myMovement('_state)) {
        let('nextDir, 'chooseFreeDir('world, '_state)) {
           (tuple('nextDir, 0, 0, 0), 'nextDir)
        }}
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

  val (prog, labels) = toProg(typecheck(main))

  //println("[" + (resolveSymbolic(prog, labels) map (_ show(true)) mkString ",\n") + "]")
  println((resolveSymbolic(prog, labels) map (_ show()) mkString "\n"))

}