package ilc
package examples

import java.io.{ File, FileWriter }

trait Generator
extends Archive
   with Dummy
{
  def main(args: Array[String]) {
    // base is the directory to generate in
    Console.err.println("Generator started")
    val Array(base, classOutput) = args take 2 map (new File(_))
    base.mkdirs()
    exportDummy(base)
    exportExamples(base, classOutput)
  }

  // stdout is exported as paths
  def export(path: String): Unit =
    Console.out.println(path)

  // stderr is echoed as [info] in sbt console
  def info(message: String): Unit =
    Console.err.println(message)

  def exportExamples(base: File, classOutput: File) {
    for {
      example <- archive.values
    } {
      exportSource(base, classOutput, example)
    }
  }

  // dummy code
  def exportDummy(base: File) {
    val path = new File(base, "DummyGenerated.scala").getCanonicalPath
    val file = new FileWriter(path)
    file.write(scalaMeterDummyCode)
    file.close
    export(path)
  }

  def exportSource(base: File, classOutput: File, example: Example) {
    for {
      src <- example.toSource(base)
      out = src.saveIfNeeded(classOutput)
    } {
      //Ensure this file is tracked by SBT.
      export(out.getCanonicalPath)
    }
  }
}
