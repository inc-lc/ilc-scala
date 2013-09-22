package ilc

/**
 * Examples generator
 *
 * Interface:
 * 1. args.head is the directory to generate into.
 * 2. for each file that's generated, print one line with it's path.
 */

import examples._

object Examples
extends Generator {
  // the compiled object is "MapSuccGenerated"
  // the benchmarking object is "MapSuccBenchmark"
  addExample("MapSucc", new MapSuccExample)

  //Ditto
  addExample("MapSuccBase", new MapSuccBaseExample)
}
   // idea for speeding up lookup/update: memoizing algorithms
