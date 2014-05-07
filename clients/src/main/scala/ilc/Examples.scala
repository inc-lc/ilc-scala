package ilc

/**
 * Examples generator
 *
 * Interface:
 * 1. the first argument is the directory to generate into.
 * 2. for each file that's generated, print one line with its path.
 */

import examples._

object Examples
extends Generator {
  // the compiled object is "MapSuccGenerated"
  // the benchmarking object is "MapSuccBenchmark"
  addExample(new MapSuccExample)

  //Ditto
  addExample(new BagUnionExample)

  addExample(new MapSuccBaseExample)

  addExample(new MapSuccBagsExample)

  addExample(new SumValuesExample)

  addExample(new GroupByExample)
}
   // idea for speeding up lookup/update: memoizing algorithms
