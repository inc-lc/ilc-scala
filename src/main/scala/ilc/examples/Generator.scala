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
      (name, example) <- archive
    } {
      exportSource(base, name, example)
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

  def exportSource(base: File, name: String, example: Example) {
    val source = example.toSource(name)
    import java.io.FileWriter
    val outFile = new File(base, source.objectName ++ ".scala")
    val writer = new FileWriter(outFile)
    writer.write(source.code)
    writer.close
    export(outFile.getCanonicalPath)
  }
}
