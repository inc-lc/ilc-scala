package ilc
package examples

import java.io.File
import ilc.feature.base.Syntax

trait Generator
extends Archive
   with Dummy
{
  def main(args: Array[String]) {
    // base is the directory to generate in
    Console.err.println("Generator started")
    val base = new File(args.head)
    base.mkdirs()
    exportDummy(base)
    exportExamples(base)
  }

  // stdout is exported as paths
  def export(path: String): Unit =
    Console.out.println(path)

  // stderr is echoed as [info] in sbt console
  def info(message: String): Unit =
    Console.err.println(message)

  def exportExamples(base: File) {
    for {
      example <- archive.values
    } {
      exportSource(base, example)
    }
  }

  // dummy code
  def exportDummy(base: File) {
    val path = new File(base, "DummyGenerated.scala").getCanonicalPath
    import java.io.FileWriter
    val file = new FileWriter(path)
    file.write(scalaMeterDummyCode)
    file.close
    export(path)
  }

  def exportSource(base: File, example: Example) {
    for {
      src <- example.toSource(base)
      out = src.saveIfNeeded()
    } {
      //Ensure this file is tracked by SBT.
      export(out.getCanonicalPath)
    }
  }
}
