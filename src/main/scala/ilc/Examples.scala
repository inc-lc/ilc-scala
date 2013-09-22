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
  // the compiled object is "MapSuccBinary"
  // the benchmarking object is "MapSuccBenchmark"
  addExample("MapSucc", new MapSuccExample)
}
   // idea for speeding up lookup/update: memoizing algorithms
